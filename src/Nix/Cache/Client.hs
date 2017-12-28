module Nix.Cache.Client where

import Control.Concurrent.Async.Lifted (wait)
import Control.Monad.State.Strict (execStateT, modify)
import GHC.Conc (getNumProcessors)
import Network.HTTP.Client (Manager, Request(..), ManagerSettings(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant ((:<|>)(..), Proxy(Proxy))
import Servant.Client (BaseUrl(..), ClientM, ClientEnv(ClientEnv), Scheme(..))
import Servant.Client (runClientM, client, )
import Servant.Common.BaseUrl (parseBaseUrl)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Environment (lookupEnv)
import System.Process.Text (readProcessWithExitCode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Nix.Cache.Common
import Nix.Cache.API
import Nix.StorePath (NixStoreDir, StorePath, PathSet)
import Nix.StorePath (ioParseFullStorePath, spToFull)
import Nix.StorePath (getNixStoreDir, abbrevSP)
import Nix.ReferenceCache
import Nix.Cache.Types
import Nix.Nar
import Nix.NarInfo
import Nix.Bin

-------------------------------------------------------------------------------
-- * Servant client
-------------------------------------------------------------------------------

-- | Define the client by pattern matching.
nixCacheInfo :: ClientM NixCacheInfo
narInfo :: NarInfoReq -> ClientM NarInfo
nar :: NarRequest -> ClientM Nar
queryPaths :: Vector FilePath -> ClientM (HashMap FilePath Bool)
sendNar :: Nar -> ClientM StorePath
nixCacheInfo
  :<|> narInfo
  :<|> nar
  :<|> queryPaths
  :<|> sendNar = client (Proxy :: Proxy NixCacheAPI)

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

-- | State for the nix client monad.
data NixClientState = NixClientState {
  -- ^ Computed store path dependency tree.
  ncsSentPaths :: !(HashMap StorePath (Async ()))
  -- ^ Mapping of store paths to asynchronous actions which send those paths.
  } deriving (Generic)

-- | Object read by the nix client reader.
data NixClientObj = NixClientObj {
  ncoConfig :: NixClientConfig,
  -- ^ Static configuration of the client.
  ncoState :: MVar NixClientState,
  -- ^ Mutable state of the client.
  ncoManager :: Manager,
  -- ^ HTTP connection manager client uses to connect.
  ncoPathReferenceCache :: NixPathReferenceCache,
  -- ^ Database connection for the local cache. Syncronized in MVar to
  -- allow lastrowid to be deterministic
  ncoLogMutex :: MVar (),
  -- ^ Syncronizes logs of the client so they don't overlap.
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

-- | Run the nix client monad.
runNixClient :: NixClient a -> IO a
runNixClient action = do
  cfg <- loadClientConfig
  semaphore <- newQSem (nccMaxWorkers cfg)
  manager <- mkManager cfg
  let state = NixClientState mempty
  stateMVar <- newMVar state
  logMVar <- newMVar ()
  cache <- newPathReferenceCache
  let obj = NixClientObj cfg stateMVar manager cache logMVar semaphore
  -- Perform the action and then update the cache.
  result <- runReaderT (action) obj
  pure result

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
  newManager managerSettings {managerModifyRequest = modifyReq}

-- | Perform a request with the servant client in the NixClient monad.
clientRequest :: ClientM a -> NixClient a
clientRequest req = do
  config <- ncoConfig <$> ask
  manager <- ncoManager <$> ask
  let env = ClientEnv manager (nccCacheUrl config)
  liftIO $ runClientM req env >>= \case
    Left err -> error $ show err
    Right result -> pure result

-------------------------------------------------------------------------------
-- * Nix client actions
-------------------------------------------------------------------------------

-- | Get references of a path, reading from and writing to a cache.
getReferences :: StorePath -> NixClient PathSet
getReferences spath = do
  cache <- ncoPathReferenceCache <$> ask
  liftIO $ getPathReferences cache spath

-- | Get the full runtime path dependency closure of a store path.
getClosure :: StorePath -> NixClient [StorePath]
getClosure path = do
  NixBinDir nixBin <- nccBinDir . ncoConfig <$> ask
  storeDir <- nccStoreDir . ncoConfig <$> ask
  let nix_store = nixBin </> "nix-store"
      args = ["-qR", spToFull storeDir path]
  liftIO $ readProcessWithExitCode nix_store args "" >>= \case
    (ExitSuccess, stdout, _) -> do
      map snd <$> mapM ioParseFullStorePath (splitWS stdout)
    (ExitFailure code, _, stderr) -> do
      error msg
      where cmd = nix_store <> " " <> intercalate " " args
            msg' = cmd <> " failed with " <> show code
            msg = msg' <> unpack (if T.strip stderr == "" then ""
                                  else "\nSTDERR:\n" <> stderr)

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
  pathsToSend <- HS.fromList . concat <$> mapM getClosure paths
  ncDebug $ "Full closure contains " <> count pathsToSend <> "."
  storeDir <- nccStoreDir . ncoConfig <$> ask
  -- Convert the path list to full paths and convert that to a vector.
  let pathsV = V.fromList $ map (spToFull storeDir) $ HS.toList pathsToSend
  -- Now that we have the full list built up, send it to the
  -- server to see which paths are already there.
  response <- clientRequest $ queryPaths pathsV
  -- Split the dictionary into two lists.
  result <- flip execStateT (mempty, mempty) $ do
    forM_ (H.toList response) $ \(pathStr, isInRepo) -> do
      spath <- snd <$> ioParseFullStorePath (pack pathStr)
      modify $ \(inrepo, notinrepo) -> case isInRepo of
        True -> (HS.insert spath inrepo, notinrepo)
        False -> (inrepo, HS.insert spath notinrepo)
  ncDebug $ count (fst result) <> " paths are already on the repo, and "
         <> count (snd result) <> " paths are not."
  pure result

-- | Send paths and their dependencies, after first checking for ones
-- already sent.
sendClosures :: [StorePath] -> NixClient ()
sendClosures spaths = do
  state <- ncoState <$> ask
  modifyMVar state $ \s -> do
    (inRepo, _) <- queryStorePaths spaths
    actions <- map H.fromList $ forM (HS.toList inRepo) $ \path -> do
      action <- async $ do
        -- Instead of a send action, just print a debug message that
        -- it was already sent.
        ncDebug $ abbrevSP path <> " was already in the repo."
      pure (path, action)
    -- Update the state, inserting no-op actions for any paths which
    -- have already been sent.
    pure (s {ncsSentPaths = ncsSentPaths s <> actions}, ())
  mapM sendClosure spaths >>= mapM_ wait

-- | Send a path and its full dependency set to a binary cache.
sendClosure :: StorePath -> NixClient (Async ())
sendClosure spath = do
  state <- asks ncoState
  modifyMVar state $ \s -> do
    case H.lookup spath $ ncsSentPaths s of
      Just action -> return (s, action)
      Nothing -> do
        action <- async $ do
          ncDebug $ "Started process to send " <> abbrevSP spath
          refs <- getReferences spath
          -- Concurrently send parent paths.
          refActions <- forM (HS.toList refs) $ \ref -> do
            rAction <- sendClosure ref
            pure (ref, rAction)
          forM_ refActions $ \(ref, rAction) -> do
            ncDebug $ concat [abbrevSP spath, " is waiting for ",
                              abbrevSP ref, " to finish sending (",
                              tshow $ asyncThreadId rAction, ")"]
            wait rAction
            ncDebug $ abbrevSP ref <> " finished"
          -- Once parents are sent, send the path itself.
          sendPath spath
        let s' = s {ncsSentPaths = H.insert spath action $ ncsSentPaths s}
        return (s', action)

-- | Send a single path to a nix repo.
sendPath :: StorePath -> NixClient ()
sendPath p = do
  -- Acquire a resource from our semaphore.
  waitQSem =<< asks ncoSemaphore
  ncDebug $ "Getting nar data for " <> abbrevSP p <> "..."
  storeDir <- nccStoreDir . ncoConfig <$> ask
  binDir <- nccBinDir . ncoConfig <$> ask
  nar <- liftIO $ getNar binDir storeDir p
  ncInfo $ "Sending " <> abbrevSP p
  clientRequest $ sendNar nar
  ncDebug $ "Finished sending " <> abbrevSP p
  -- Release the resource to the semaphore.
  signalQSem =<< asks ncoSemaphore
