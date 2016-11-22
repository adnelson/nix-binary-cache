module Nix.Cache.Client where

import Servant.Client (BaseUrl(..), client, ServantError, Scheme(..))
import Network.HTTP.Client (Manager, Request(..), ManagerSettings(..),
                            newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Servant
import Servant.Common.BaseUrl (parseBaseUrl)
import System.Process (readCreateProcess, shell)
import System.Directory (createDirectoryIfMissing,
                         renameDirectory,
                         doesDirectoryExist)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import System.Environment (getEnv, lookupEnv)
import Control.Concurrent.Async.Lifted (wait)
import GHC.Conc (getNumProcessors)

import Nix.Cache.Common
import Nix.Cache.API
import Nix.StorePath
import Nix.Cache.Types
import Nix.Nar

-------------------------------------------------------------------------------
-- * Servant client
-------------------------------------------------------------------------------

-- Make a client request returning a `t`.
type ClientReq t = Manager -> BaseUrl -> ExceptT ServantError IO t

-- | Define the client by pattern matching.
nixCacheInfo :: ClientReq NixCacheInfo
narInfo :: NarInfoReq -> ClientReq NarInfo
nar :: NarRequest -> ClientReq Nar
queryPaths :: Vector FilePath -> ClientReq (HashMap FilePath Bool)
sendNar :: Nar -> ClientReq StorePath
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
nixCacheUrlFromEnv = getEnv "NIX_REPO_HTTP" >>= parseBaseUrl

-- | A dependency tree, represented as a mapping from a store path to
-- its set of (immediate, not transitive) dependent paths.
type PathTree = HashMap StorePath [StorePath]

-- | A set of store paths.
type PathSet = HashSet StorePath

-------------------------------------------------------------------------------
-- * Local filesystem cache for path references
-------------------------------------------------------------------------------

-- | Write a path tree to the cache.
-- Iterates through keys of the path tree, for each one creates a
-- directory for the base path of the store path, and touches files
-- corresponding to paths of its dependencies.
-- So for example, if /nix/store/xyz-foo depends on /nix/store/{a,b,c},
-- then we will create
--   cacheLocation/xyz-foo/a
--   cacheLocation/xyz-foo/b
--   cacheLocation/xyz-foo/c
writeCache :: NixClient ()
writeCache = do
  cacheLocation <- nccCacheLocation <$> ncoConfig <$> ask
  mv <- ncoState <$> ask
  tree <- ncsPathTree <$> readMVar mv
  liftIO $ createDirectoryIfMissing True cacheLocation
  forM_ (H.toList tree) $ \(path, refs) -> do
    writePathCache path refs

-- Write the on-disk cache entry for a single path.
writePathCache :: StorePath -> [StorePath] -> NixClient ()
writePathCache path refs = do
  cacheLocation <- nccCacheLocation <$> ncoConfig <$> ask
  let pathDir = cacheLocation </> spToPath path
  liftIO (doesDirectoryExist pathDir) >>= \case
    True -> pure ()
    False -> liftIO $ do
      -- Build the cache in a temporary directory, and then move it
      -- to the actual location. This prevents the creation of a
      -- partial directory in the case of a crash.
      tempDir <- T.unpack . T.strip . T.pack <$>
        readCreateProcess (shell $ "mktemp -d " <> pathDir <> "XXXX") ""
      forM_ refs $ \ref -> do
        writeFile (tempDir </> spToPath ref) ("" :: String)
      renameDirectory tempDir pathDir
      -- Make the directory read-only.
      readCreateProcess (shell $ "chmod 0555 " <> pathDir) ""
      pure ()

-- | Read a cache from disk into memory.
readCache :: FilePath -> IO PathTree
readCache cacheLocation = doesDirectoryExist cacheLocation >>= \case
  False -> pure mempty
  True -> do
    paths <- listDirectory cacheLocation
    map H.fromList $ forM paths $ \path -> do
      bpath <- ioParseStorePath $ pack path
      deps <- do
        depfiles <- listDirectory (cacheLocation </> path)
        mapM (ioParseStorePath . pack) depfiles
      pure (bpath, deps)


-------------------------------------------------------------------------------
-- * Nix client monad
-------------------------------------------------------------------------------

-- | Configuration of the nix client.
data NixClientConfig = NixClientConfig {
  nccStoreDir :: NixStoreDir,
  -- ^ Location of the nix store.
  nccCacheLocation :: FilePath,
  -- ^ Location of the nix client path cache.
  nccCacheUrl :: BaseUrl,
  -- ^ Base url of the nix binary cache.
  nccCacheAuth :: Maybe NixCacheAuth,
  -- ^ Optional auth for the nix cache, if using HTTPS.
  nccLogLevel :: LogLevel
  -- ^ Minimum level of logging messages to show.
  } deriving (Show, Generic)

-- | State for the nix client monad.
data NixClientState = NixClientState {
  ncsPathTree :: PathTree,
  -- ^ Computed store path dependency tree.
  ncsSentPaths :: HashMap StorePath (Async ())
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
  ncoLogMutex :: MVar (),
  -- ^ Syncronizes logs of the client so they don't overlap.
  ncoSemaphore :: QSem
  }

-- | Nix client monad.
type NixClient = ReaderT NixClientObj IO

-- | Run the nix client monad.
runNixClient :: NixClient a -> IO a
runNixClient action = do
  storeDir <- getNixStoreDir
  home <- getEnv "HOME"
  cacheUrl <- nixCacheUrlFromEnv
  cacheAuth <- authFromEnv
  maxWorkers <- (>>= readMay) <$> lookupEnv "MAX_WORKERS" >>= \case
    Just n | n > 0 -> return n
    _ -> getNumProcessors
  minLogLevel <- (>>= readMay) <$> lookupEnv "LOG_LEVEL" >>= \case
    Just n | n >= 0 -> return n
    _ -> return _LOG_INFO
  semaphore <- newQSem maxWorkers
  let cfg = NixClientConfig {
        nccStoreDir = storeDir,
        nccCacheLocation = home </> ".nix-path-cache",
        nccCacheUrl = cacheUrl,
        nccCacheAuth = cacheAuth,
        nccLogLevel = minLogLevel
        }
  manager <- mkManager cfg
  pathTree <- readCache $ nccCacheLocation cfg
  let state = NixClientState pathTree mempty
  stateMVar <- newMVar state
  logMVar <- newMVar ()
  -- TODO make this configurable
  let obj = NixClientObj cfg stateMVar manager logMVar semaphore
  -- Perform the action and then update the cache.
  result <- runReaderT (action <* writeCache) obj
  pure result

-------------------------------------------------------------------------------
-- * Nix client logging
-------------------------------------------------------------------------------

type LogLevel = Int

_LOG_DEBUG, _LOG_INFO, _LOG_WARN, _LOG_FATAL :: LogLevel
(_LOG_DEBUG, _LOG_INFO, _LOG_WARN, _LOG_FATAL) = (15, 30, 45, 60)

-- | Logger. Writes to stdout and ignores level for now. Writes are mutexed.
ncLog :: LogLevel -> Text -> NixClient ()
ncLog level message = do
  minlevel <- nccLogLevel . ncoConfig <$> ask
  when (level >= minlevel) $ do
    logmv <- ncoLogMutex <$> ask
    withMVar logmv $ \_ -> putStrLn message

ncDebug :: Text -> NixClient ()
ncDebug = ncLog _LOG_DEBUG

ncInfo :: Text -> NixClient ()
ncInfo = ncLog _LOG_INFO

ncWarn :: Text -> NixClient ()
ncWarn = ncLog _LOG_WARN

ncFatal :: Text -> NixClient ()
ncFatal = ncLog _LOG_FATAL

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
clientRequest :: ClientReq a -> NixClient a
clientRequest req = do
  config <- ncoConfig <$> ask
  manager <- ncoManager <$> ask
  let baseUrl = nccCacheUrl config
  liftIO $ runExceptT (req manager baseUrl) >>= \case
    Left err -> error $ show err
    Right result -> pure result

-------------------------------------------------------------------------------
-- * Nix client actions
-------------------------------------------------------------------------------

-- | Get the references of an object by asking the nix-store. This
-- information is cached by the caller of this function.
getReferences' :: StorePath -> NixClient [StorePath]
getReferences' spath = do
  storeDir <- nccStoreDir <$> ncoConfig <$> ask
  let cmd = "nix-store --query --references " <> spToFull storeDir spath
  result <- liftIO $ pack <$> readCreateProcess (shell $ unpack cmd) ""
  forM (splitWS result) $ \line -> case parseFullStorePath line of
    Left err -> error err
    Right (_, sp) -> pure sp

-- | Get references of a path, reading from and writing to a cache.
getReferences :: StorePath -> NixClient [StorePath]
getReferences spath = do
  mv <- asks ncoState
  modifyMVar mv $ \s -> do
    let t = ncsPathTree s
    case lookup spath t of
      Just refs -> pure (s, refs)
      Nothing -> do
        refs' <- getReferences' spath
        -- Filter out self-referential paths.
        let refs = filter (/= spath) refs'
        pure (s {ncsPathTree = H.insert spath refs t}, refs)

-- | Given some store paths to send, find their closure and see which
-- of those paths need to be sent to the server.
queryStorePaths :: [StorePath] -- ^ Top-level store paths to send.
                -> NixClient PathSet -- ^ Store paths that need to be sent.
queryStorePaths paths = do
  pathsToSend <- newMVar mempty
  -- Define a recursive function which will concurrently fetch a
  -- dependency tree of a list of paths.
  let loop _paths = mapConcurrently getRefs _paths >> return ()
      getRefs path = unlessM (elem path <$> readMVar pathsToSend) $ do
        modifyMVar_ pathsToSend $ pure . HS.insert path
        loop =<< getReferences path
  -- Call the loop on the input path list.
  loop paths
  pathList <- HS.toList <$> readMVar pathsToSend
  storeDir <- nccStoreDir . ncoConfig <$> ask
  -- Convert the path list to full paths and convert that to a vector.
  let pathsV = V.fromList $ map (spToFull storeDir) pathList
  -- Now that we have the full list built up, send it to the
  -- server to see which paths are already there.
  result <- clientRequest $ queryPaths pathsV
  -- Each of the keys for which the value is False are not on the server.
  -- In addition, we filter out any paths that we fail to parse.
  let spaths = flip map (H.keys $ H.filter not result) $ \path -> do
        case parseFullStorePath $ pack path of
          Right (_, spath) -> Just spath
          Left _ -> Nothing
  -- Convert the not-found paths to a set and return it.
  pure $ HS.fromList $ catMaybes spaths

-- | Send a path and its full dependency set to a binary cache.
sendClosure :: StorePath -> NixClient (Async ())
sendClosure spath = do
  state <- asks ncoState
  modifyMVar state $ \s -> do
    case H.lookup spath $ ncsSentPaths s of
      Just action -> return (s, action)
      Nothing -> do
        action <- async $ do
          refs <- getReferences spath
          -- Concurrently send parent paths.
          refActions <- forM refs $ \ref -> do
            rAction <- sendClosure ref
            pure (ref, rAction)
          forM_ refActions $ \(ref, rAction) -> do
            ncDebug $ "Waiting for " <> pack (spToPath ref) <> " to finish sending..."
            wait rAction
          -- Once parents are sent, send the path itself.
          sendPath spath
        let s' = s {ncsSentPaths = H.insert spath action $ ncsSentPaths s}
        return (s', action)

-- | Send a single path to a nix repo.
sendPath :: StorePath -> NixClient ()
sendPath p = do
  -- Acquire a resource from our semaphore.
  waitQSem =<< asks ncoSemaphore
  ncDebug $ "Getting nar data for " <> tp <> "..."
  storeDir <- nccStoreDir . ncoConfig <$> ask
  nar <- liftIO $ getNar storeDir p
  ncInfo $ "Sending " <> tp
  clientRequest $ sendNar nar
  ncDebug $ "Finished sending " <> tp
  -- Release the resource to the semaphore.
  signalQSem =<< asks ncoSemaphore
  where
    tp = pack $ spToPath p
