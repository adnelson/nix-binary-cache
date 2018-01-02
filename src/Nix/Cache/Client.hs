module Nix.Cache.Client where

import Control.Concurrent.Async.Lifted (wait)
import Control.Monad.State.Strict (execStateT, modify)
import GHC.Conc (getNumProcessors)
import Network.HTTP.Client (Manager, Request(..), ManagerSettings(..))
import Network.HTTP.Client (newManager, defaultManagerSettings, responseHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant ((:<|>)(..), Proxy(Proxy))
import Servant.Client (BaseUrl(..), ClientM, ClientEnv(ClientEnv), Scheme(..))
import Servant.Client (runClientM, client, ServantError)
import Servant.Common.BaseUrl (parseBaseUrl)
import System.Environment (lookupEnv)
import System.IO (IO, stdout, stderr, hSetBuffering, BufferMode(LineBuffering))
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Prelude as P

import Nix.Cache.Common
import Nix.Bin (NixBinDir, getNixBinDir, nixCmd)
import Nix.Cache.API (NixCacheAPI)
import Nix.Cache.Client.Misc (getNixStorePaths)
import Nix.Cache.Types (NixCacheInfo(storeDir))
import Nix.Derivation -- (Derivation(..), parseDeriv
import Nix.Nar (Nar, NarExport, getNarExport, importNarExport)
import Nix.NarInfo (NarInfo(references, narReq), NarInfoReq(..), NarRequest)
import Nix.ReferenceCache (ReferenceCache, addPath)
import Nix.ReferenceCache (computeClosure, recordReferences, getReferences)
import Nix.ReferenceCache (newReferenceCache, initializeReferenceCache)
import Nix.StorePath (NixStoreDir, StorePath(spPrefix), PathSet)
import Nix.StorePath (getNixStoreDir, abbrevSP)
import Nix.StorePath (ioParseFullStorePath, ioParseStorePath, spToFull)

-------------------------------------------------------------------------------
-- * Servant client
-------------------------------------------------------------------------------

-- | Define the client by pattern matching.
fetchNixCacheInfo :: ClientM NixCacheInfo
fetchNarInfo :: NarInfoReq -> ClientM NarInfo
fetchNar :: NarRequest -> ClientM Nar
queryPaths :: Vector FilePath -> ClientM (HashMap FilePath Bool)
sendNarExport :: NarExport -> ClientM StorePath
fetchNixCacheInfo
  :<|> fetchNarInfo
  :<|> fetchNar
  :<|> queryPaths
  :<|> sendNarExport = client (Proxy :: Proxy NixCacheAPI)

-- | Base URL of the nixos cache.
nixosCacheUrl :: BaseUrl
nixosCacheUrl = BaseUrl {
  baseUrlScheme = Https,
  baseUrlHost = "cache.nixos.org",
  baseUrlPort = 443,
  baseUrlPath = ""
  }

-- | Nix cache auth.
data NixCacheAuth = NixCacheAuth Text Text
  deriving (Show, Eq, Generic)

-- | Read auth from the environment.
authFromEnv :: IO (Maybe NixCacheAuth)
authFromEnv = do
  username <- map T.pack <$> lookupEnv "NIX_BINARY_CACHE_USERNAME"
  password <- map T.pack <$> lookupEnv "NIX_BINARY_CACHE_PASSWORD"
  case (username, password) of
    (Just "", Just _) -> pure Nothing
    (Just user, Just pass) -> pure $ Just $ NixCacheAuth user pass
    _ -> pure Nothing

-- | Read nix cache url from the environment.
nixCacheUrlFromEnv :: IO BaseUrl
nixCacheUrlFromEnv = do
  base <- lookupEnv "NIX_REPO_HTTP" >>= \case
    Nothing -> pure "https://cache.nixos.org"
    Just url -> pure url
  parseBaseUrl base

-------------------------------------------------------------------------------
-- * Nix client monad
-------------------------------------------------------------------------------

data NixClientError
  -- | If we got an HTTP error when uploading a path.
  = PathFailedToSend StorePath ServantError
  -- | If we got an HTTP error when downloading a path.
  | PathFailedToFetch StorePath ServantError
  -- | If we got an HTTP error when requesting path NAR info.
  | PathFailedToGetNarInfo StorePath ServantError
  -- | If we expected the server to have a NARInfo but it doesn't.
  | PathNotOnServer StorePath
  -- | If we expected the client to have a store path but it doesn't.
  | PathNotOnClient StorePath
  -- | If we got an HTTP error when requesting nix cache info.
  | FailedToReadNixCacheInfo ServantError
  -- | If the remote nix store directory is not the same as the local.
  | StoreDirectoryMismatch { remoteStoreDir :: NixStoreDir,
                             localStoreDir :: NixStoreDir }
  deriving (Show, Eq, Generic)

instance Exception NixClientError

-- | Configuration of the nix client.
data NixClientConfig = NixClientConfig {
  nccStoreDir :: NixStoreDir,
  -- ^ Location of the nix store.
  nccBinDir :: NixBinDir,
  -- ^ Location of nix binaries.
  nccCacheUrl :: BaseUrl,
  -- ^ Base url of the nix binary cache.
  nccCacheAuth :: Maybe NixCacheAuth,
  -- ^ Optional auth for the nix cache, if using HTTPS.
  nccMaxWorkers :: Int,
  -- ^ Max number of concurrent tasks
  nccLogLevel :: LogLevel
  -- ^ Minimum level of logging messages to show.
  } deriving (Show, Generic)

-- | Create a particular kind of async action for sending a path
newtype AsyncSend = AsyncSend (Async ())

-- | Create a particular kind of async action for fetching a path
newtype AsyncFetch = AsyncFetch (Async ())

-- | State for the nix client monad.
data NixClientState = NixClientState {
  -- | Mapping of store paths to asynchronous actions which send those paths.
  ncsSentPaths :: MVar (HashMap StorePath AsyncSend),
  -- | Mapping of store paths to asynchronous actions which fetch those paths.
  ncsFetchedPaths :: MVar (HashMap StorePath AsyncFetch),
  -- | Starts out as Nothing and becomes Just Nothing if valid, and
  -- Just (Just err) if there's an error.
  ncsValidatedStoreDirectory :: MVar (Maybe (Maybe NixClientError))
  } deriving (Generic)

-- | Object read by the nix client reader.
data NixClientObj = NixClientObj {
  -- | Static configuration of the client.
  ncoConfig :: NixClientConfig,
  -- | Mutable state of the client.
  ncoState :: NixClientState,
  -- | HTTP connection manager client uses to connect.
  ncoManager :: Manager,
  -- | Database connection for the local cache. Syncronized in MVar to
  -- allow lastrowid to be deterministic
  ncoReferenceCache :: ReferenceCache,
  -- | Syncronizes logs of the client so they don't overlap.
  ncoLogMutex :: MVar (),
  -- | Limits the number of concurrent client operations.
  ncoSemaphore :: QSem
  }

-- | Nix client monad.
type NixClient = ReaderT NixClientObj IO

loadClientConfig :: IO NixClientConfig
loadClientConfig = do
  nccStoreDir <- getNixStoreDir
  nccCacheUrl <- nixCacheUrlFromEnv
  nccCacheAuth <- authFromEnv
  nccMaxWorkers <- (>>= readMay) <$> lookupEnv "MAX_WORKERS" >>= \case
    Just n | n > 0 -> return n
    _ -> getNumProcessors
  nccLogLevel <- (>>= readMay) <$> lookupEnv "LOG_LEVEL" >>= \case
    Just level -> pure level
    _ -> return LOG_INFO
  nccBinDir <- getNixBinDir
  pure NixClientConfig {..}

-- | Run the nix client monad, given a configuration.
runNixClientWithConfig :: NixClientConfig -> NixClient a -> IO a
runNixClientWithConfig cfg action = do
  semaphore <- newQSem (nccMaxWorkers cfg)
  manager <- mkManager cfg
  state <- NixClientState <$> newMVar mempty <*> newMVar mempty
                          <*> newMVar Nothing
  logMVar <- newMVar ()
  cache <- newReferenceCache
  initializeReferenceCache cache
  let obj = NixClientObj cfg state manager cache logMVar semaphore
  -- Perform the action and then update the cache.
  result <- runReaderT (action) obj
  pure result

-- | Run the nix client monad.
runNixClient :: NixClient a -> IO a
runNixClient action = do
  cfg <- loadClientConfig
  runNixClientWithConfig cfg action

-------------------------------------------------------------------------------
-- * Nix client logging
-------------------------------------------------------------------------------

-- | Four levels of logging.
data LogLevel
  = LOG_DEBUG | LOG_INFO | LOG_WARN | LOG_FATAL
  deriving (Show, Read, Eq, Ord)

-- | Logger. Writes to stdout and checks level to see if it should
-- print. Writes are mutexed so that it's threadsafe.
ncLog :: LogLevel -> Text -> NixClient ()
ncLog level message = do
  minlevel <- nccLogLevel . ncoConfig <$> ask
  when (level >= minlevel) $ do
    logmv <- ncoLogMutex <$> ask
    withMVar logmv $ \_ -> do
      case level <= LOG_DEBUG of
        True -> do
          tid <- tshow <$> myThreadId
          putStrLn $ tid <> ": " <> message
        False -> do
          putStrLn message

ncDebug :: Text -> NixClient ()
ncDebug = ncLog LOG_DEBUG

ncInfo :: Text -> NixClient ()
ncInfo = ncLog LOG_INFO

ncWarn :: Text -> NixClient ()
ncWarn = ncLog LOG_WARN

ncFatal :: Text -> NixClient ()
ncFatal = ncLog LOG_FATAL

-------------------------------------------------------------------------------
-- * Nix client HTTP configuration and interaction
-------------------------------------------------------------------------------

-- | Given some configuration, create the request manager.
mkManager :: NixClientConfig -> IO Manager
mkManager config = do
  let baseUrl = nccCacheUrl config
      mauth = nccCacheAuth config
      managerSettings = case baseUrlScheme baseUrl of
        Https -> tlsManagerSettings
        _ -> defaultManagerSettings
      -- A request modifier function, which adds the username/password
      -- to the Authorization header.
      modifyReq req = pure $ case mauth of
        Nothing -> req
        Just (NixCacheAuth username password) -> do
          -- Encode the username:password in base64.
          let auth = username <> ":" <> password
          let authB64 = B64.encode $ T.encodeUtf8 auth
          req {
            requestHeaders = requestHeaders req `snoc`
              ("Authorization", "Basic " <> authB64)
          }
      modifyResponse resp = do
        let headers = H.fromList $ responseHeaders resp
            fixContentType = \case
              "binary/octet-stream" -> "application/octet-stream"
              "text/x-nix-narinfo" -> "application/octet-stream"
              "application/x-nix-nar" -> "application/octet-stream"
              ctype -> ctype
            headers' = case lookup "content-type" headers of
              Just t -> H.insert "content-type" (fixContentType t) headers
              Nothing -> headers
        pure resp {responseHeaders = H.toList headers'}
  newManager managerSettings {
    managerModifyRequest = modifyReq,
    managerModifyResponse = modifyResponse
    }

-- | Perform a request with the servant client in the NixClient monad.
-- Throws error responses as exceptions.
clientRequest :: ClientM a -> NixClient a
clientRequest req = clientRequestEither req >>= \case
  Left err -> throw err
  Right result -> pure result

-- | Perform a request with the servant client in the NixClient monad.
clientRequestEither :: ClientM a -> NixClient (Either ServantError a)
clientRequestEither req = validateStoreDirectory >> do
  config <- ncoConfig <$> ask
  manager <- ncoManager <$> ask
  let env = ClientEnv manager (nccCacheUrl config)
  ncDebug $ "Making request!"
  liftIO $ runClientM req env

-- | Validate that the remote cache has the same store directory as
-- the client. This is only performed once, and it's lazy: it only
-- happens if we need to communicate with the remote server (which isn't
-- always the case)
validateStoreDirectory :: NixClient ()
validateStoreDirectory = do
  mv <- ncsValidatedStoreDirectory <$> ncoState <$> ask
  readMVar mv >>= \case
    -- Already been validated, just return
    Just Nothing -> pure ()
    -- Encountered an error before, throw that error
    Just (Just err) -> throw err
    -- Hasn't been done yet, validate and fill the mvar
    Nothing -> do
      modifyMVar_ mv $ \_ -> do
        ncDebug "Validating remote nix store directory..."
        config <- ncoConfig <$> ask
        manager <- ncoManager <$> ask
        let env = ClientEnv manager (nccCacheUrl config)
        liftIO (runClientM fetchNixCacheInfo env) >>= \case
          Left err -> pure (Just $ Just $ FailedToReadNixCacheInfo err)
          Right info -> do
            let remoteStoreDir = storeDir info
            localStoreDir <- nccStoreDir . ncoConfig <$> ask
            case remoteStoreDir == localStoreDir of
              True -> do
                ncDebug $ "Validated: " <> tshow localStoreDir
                pure (Just Nothing)
              False -> pure (Just $ Just StoreDirectoryMismatch {..})
      -- Now that we checked, rerun the validate command
      validateStoreDirectory

-------------------------------------------------------------------------------
-- * Nix client actions
-------------------------------------------------------------------------------

-- | Do a nix client action inside of the semaphore
inSemaphore :: NixClient a -> NixClient a
inSemaphore action = bracket getSem releaseSem (\_ -> action) where
  -- Acquire a resource from our semaphore.
  getSem = do
    ncDebug "Waiting for semaphore"
    waitQSem =<< asks ncoSemaphore
  -- Release the resource to the semaphore.
  releaseSem () = do
    ncDebug "Releasing semaphore"
    signalQSem =<< asks ncoSemaphore

-- | Get the NAR info for a store path by requesting to the server.
getNarInfo :: StorePath -> NixClient (Maybe NarInfo)
getNarInfo path = inSemaphore $ do
  ncDebug $ "Requesting narinfo for " <> tshow path
  clientRequestEither (fetchNarInfo $ NarInfoReq (spPrefix path)) >>= \case
    Right info -> pure $ Just info
    Left err -> case errorIs404 err of
      True -> pure Nothing
      False -> throw $ PathFailedToGetNarInfo path err

-- | If the server in question doesn't support the /query-paths route,
-- instead the NarInfo of each path can be requested from the server as a
-- way to determine if it's on the server.
getPathsOnServerWithNarInfo :: [StorePath] -> NixClient (HashMap StorePath Bool)
getPathsOnServerWithNarInfo paths = do
  asyncs <- forM paths $ \path -> do
    async $ getNarInfo path >>= \case
      Nothing -> pure (path, False)
      Just _ -> pure (path, True)
  H.fromList <$> mapM wait asyncs

-- | Given some store paths to send, find their closure and see which
-- of those paths do not already exist on the server.
queryStorePaths :: [StorePath]
                -- ^ Top-level store paths to send.
                -> NixClient (PathSet, PathSet)
                -- ^ Set of paths on the server, and not on the server.
queryStorePaths paths = do
  let count paths = len <> " path" <> if len == "1" then "" else "s"
        where len = tshow $ length paths
  ncDebug $ "Computing full closure of " <> count paths <> "."
  cache <- ncoReferenceCache <$> ask
  pathsToSend <- map (HS.fromList . concat) $ forM paths $ \path -> do
    liftIO $ computeClosure cache path
  ncDebug $ "Full closure contains " <> count pathsToSend <> "."
  storeDir <- nccStoreDir . ncoConfig <$> ask
  -- Convert the path list to full paths and convert that to a vector.
  let paths = HS.toList pathsToSend
      pathsV = V.fromList $ map (spToFull storeDir) paths
  -- Now that we have the full list built up, send it to the
  -- server to see which paths are already there.
  ncDebug "Querying repo to see what paths it has"
  pathResults <- clientRequestEither (queryPaths $ pathsV) >>= \case
    Right r -> map H.fromList $ forM (H.toList r) $ \(pathStr, onServer) -> do
      putStrLn "TURDBURGER"
      spath <- snd <$> ioParseFullStorePath (pack pathStr)
      pure (spath, onServer)
    Left err -> case errorIs404 err of
      True -> getPathsOnServerWithNarInfo paths
      False -> throw err
  -- Split the dictionary into two lists.
  result <- flip execStateT (mempty, mempty) $ do
    forM_ (H.toList pathResults) $ \(spath, isInRepo) -> do
      modify $ \(inrepo, notinrepo) -> case isInRepo of
        True -> (HS.insert spath inrepo, notinrepo)
        False -> (inrepo, HS.insert spath notinrepo)
  ncInfo $ count (fst result) <> " paths are already on the repo, and "
        <> count (snd result) <> " paths are not."
  pure result

-- | Get the references of a path, using the server as a
-- fallback. This is necessary when fetching a path, because might not
-- know ahead of time what that path's references will be.
-- If the server has the path's information, cache it the same way we
-- cache normal references. Otherwise, it's an error.
getReferencesFallBackToServer :: StorePath -> NixClient PathSet
getReferencesFallBackToServer spath = do
  ncDebug $ "Getting references for " <> abbrevSP spath
  cache <- ncoReferenceCache <$> ask
  liftIO (getReferences cache spath) >>= \case
    Just refs -> pure refs
    Nothing -> getNarInfo spath >>= \case
      Just info -> do
        let refs = references info
        refPaths <- map HS.fromList $ forM refs $ \path -> do
          putStrLn $ "FARTBOMB: " <> tshow path
          ioParseStorePath (pack path)
        liftIO $ do
          addPath cache spath
          recordReferences cache spath refPaths
        pure refPaths
      Nothing -> throw $ PathNotOnServer spath

-- | Get the references of a store path, using only local information.
getReferencesLocally :: StorePath -> NixClient PathSet
getReferencesLocally spath = do
  ncDebug $ "Getting references for " <> abbrevSP spath <> " (local)"
  cache <- ncoReferenceCache <$> ask
  liftIO (getReferences cache spath) >>= \case
    Nothing -> throw $ PathNotOnClient spath
    Just refs -> pure refs

-- | Fetch the closure of a store path from the remote. This assumes
-- that the path (and its dependencies of course) is already on the
-- remote server.
fetchPath :: StorePath -> NixClient AsyncFetch
fetchPath spath = do
  state <- asks ncoState
  modifyMVar (ncsFetchedPaths state) $ \s -> do
    -- See if we've already created a thread for this fetch.
    case H.lookup spath s of
      -- If one exists just return that.
      Just action -> return (s, action)
      Nothing -> do
        action <- map AsyncFetch $ async $ do
          ncDebug $ "Started process to fetch " <> abbrevSP spath
          refs <- getReferencesFallBackToServer spath
          -- Concurrently fetch parent paths.
          refActions <- forM (HS.toList refs) $ \ref -> do
            (,) ref <$> fetchPath ref
          forM_ refActions $ \(ref, AsyncFetch rAction) -> do
            ncDebug $ concat [abbrevSP spath, " is waiting for ",
                              abbrevSP ref, " to finish fetching (",
                              tshow $ asyncThreadId rAction, ")"]
            wait rAction
            ncDebug $ abbrevSP ref <> " finished"
          ncDebug $ "Parent paths have finished fetching, now fetching "
                 <> abbrevSP spath <> " itself..."
          -- Once parents are fetched, fetch the path itself.
          fetchSinglePath spath
        return (H.insert spath action s, action)

-- | Given a derivation and optionally a subset of outputs needed from
-- that derivation, fetch all of the paths (and their dependencies)
-- from the repo. This will fetch zero or more paths -- if a path
-- isn't on the repo, it's not an error; it simply won't be fetched
-- (unless the repo reports the path as being on the server when it
-- really isn't).
fetchDerivation :: DerivationAndOutputs -> NixClient ()
fetchDerivation (DerivationAndOutputs deriv outs) = do
  paths <- case outs of
    Nothing -> pure $ fst <$> H.elems (derivOutputs deriv)
    Just outNames -> forM outNames $ \(name::OutputName) -> do
      case lookupOutput deriv name of
        Right (path :: StorePath) -> pure path
        Left err -> throw err
  fetchPaths paths

-- | Fetch paths and their dependencies, after first checking for ones
-- already present.
fetchPaths :: [StorePath] -> NixClient ()
fetchPaths spaths = do
  state <- ncoState <$> ask
  modifyMVar_ (ncsFetchedPaths state) $ \fetched -> do
    -- Get a set of store paths the repo already has.
    inRepo <- pure mempty -- fst <$> queryStorePaths spaths
    -- Create no-op actions for all of these.
    noopActions <- map H.fromList $ forM (HS.toList inRepo) $ \path -> do
      noopAction <- map AsyncFetch $ async $ do
        ncDebug $ abbrevSP path <> " was already in the repo."
      pure (path, noopAction)
    -- Update the state, inserting no-op actions for any paths which
    -- have already been sent.
    pure (fetched <> noopActions)
  -- For the unsent paths, asynchronously fetch each one, and then
  -- wait for the result.
  fetches <- mapM fetchPath spaths
  forM_ fetches $ \(AsyncFetch action) -> wait action

-- | Send paths and their dependencies, after first checking for ones
-- already sent.
sendPaths :: [StorePath] -> NixClient ()
sendPaths spaths = do
  state <- ncoState <$> ask
  modifyMVar_ (ncsSentPaths state) $ \sent -> do
    -- Get a set of store paths the repo already has.
    inRepo <- fst <$> queryStorePaths spaths
    -- Create no-op actions for all of these.
    noopActions <- map H.fromList $ forM (HS.toList inRepo) $ \path -> do
      noopAction <- map AsyncSend $ async $ do
        ncDebug $ abbrevSP path <> " was already in the repo."
      pure (path, noopAction)
    -- Update the state, inserting no-op actions for any paths which
    -- have already been sent.
    pure (sent <> noopActions)
  -- For the unsent paths, asynchronously fetch each one, and then
  -- wait for the result.
  sends <- mapM sendPath spaths
  forM_ sends $ \(AsyncSend action) -> wait action

-- | Send a path and its full dependency set to a binary cache.
sendPath :: StorePath -> NixClient AsyncSend
sendPath spath = do
  state <- asks ncoState
  modifyMVar (ncsSentPaths state) $ \s -> do
    -- See if we've already created a thread for this send.
    case H.lookup spath s of
      -- If one exists just return that.
      Just action -> return (s, action)
      Nothing -> do
        action <- map AsyncSend $ async $ do
          ncDebug $ "Started process to send " <> abbrevSP spath
          refs <- getReferencesLocally spath
          -- Concurrently send parent paths.
          refActions <- forM (HS.toList refs) $ \ref -> do
            (,) ref <$> sendPath ref
          forM_ refActions $ \(ref, AsyncSend rAction) -> do
            ncDebug $ concat [abbrevSP spath, " is waiting for ",
                              abbrevSP ref, " to finish sending (",
                              tshow $ asyncThreadId rAction, ")"]
            wait rAction
            ncDebug $ abbrevSP ref <> " finished"
          ncDebug $ "Parent paths have finished sending, now sending "
                 <> abbrevSP spath <> " itself..."
          -- Once parents are sent, send the path itself.
          sendSinglePath spath
        return (H.insert spath action s, action)

-- | Send a single path to a nix repo. This doesn't automatically
-- include parent paths, so it generally shouldn't be used externally to
-- this module (use sendPath instead)
--
-- TODO: retry logic? progress?
sendSinglePath :: StorePath -> NixClient ()
sendSinglePath p = inSemaphore $ do
  ncDebug $ "Getting nar data for " <> abbrevSP p <> "..."
  storeDir <- nccStoreDir . ncoConfig <$> ask
  binDir <- nccBinDir . ncoConfig <$> ask
  export <- liftIO $ getNarExport binDir storeDir p
  ncInfo $ "Sending " <> abbrevSP p
  clientRequestEither (sendNarExport export) >>= \case
    Right _ -> pure ()
    Left err -> throw $ PathFailedToSend p err
  ncDebug $ "Finished sending " <> abbrevSP p

-- | Fetch a single path from a nix repo. This doesn't automatically
-- include parent paths, so it generally shouldn't be used externally to
-- this module (use fetchPath instead)
--
-- TODO: retry logic? progress?
fetchSinglePath :: StorePath -> NixClient ()
fetchSinglePath p = inSemaphore $ do
  ncDebug $ "Getting nar request URL for " <> abbrevSP p <> "..."
  req <- getNarInfo p >>= \case
    Nothing -> throw $ PathNotOnServer p
    Just info -> pure $ narReq info
  -- Use this request to fetch the store path.
  clientRequestEither (fetchNar req) >>= \case
    Left err -> throw $ PathFailedToFetch p err
    Right nar -> do
      let makeNarExport = undefined
      export <- makeNarExport p nar
      binDir <- nccBinDir . ncoConfig <$> ask
      ncInfo $ "Importing " <> abbrevSP p
      liftIO $ importNarExport binDir export
      ncDebug $ "Finished fetching " <> abbrevSP p

mytest :: IO ()
mytest = do
  -- Make line-buffered, so we can use putStrLn in multithreaded code.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  cfg' <- loadClientConfig
  let cfg = cfg' { --nccCacheUrl = BaseUrl Https "green-slashnix-repo.n-s.us" 443 "",
                  nccLogLevel = LOG_DEBUG
                 }
  derivAndOuts <- nixCmd (nccBinDir cfg') "instantiate" [
    "/Users/anelson/nixpkgs", "-A", "sqlite"
    ] ""

  runNixClientWithConfig cfg (fetchDerivation derivAndOuts *> ncInfo "finished")
  -- _ <- (P.!! 5) <$> getNixStorePaths 10
  -- paths <- getNixStorePaths 20
  -- runNixClientWithConfig cfg (sendPaths paths *> ncInfo "finished")
