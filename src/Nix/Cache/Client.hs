module Nix.Cache.Client where

import Servant.Client (BaseUrl(..), client, ServantError, Scheme(..))
import Network.HTTP.Client (Manager)
import Control.Monad.Trans.Except (ExceptT)
import Servant
import System.Process (readCreateProcess, shell)
import System.Directory (createDirectoryIfMissing,
                         getDirectoryContents, renameDirectory,
                         doesDirectoryExist)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import System.Environment (getEnv)

import Nix.Cache.Common
import Nix.Cache.API
import Nix.StorePath
import Nix.Cache.Types

-- Make a client request returning a `t`.
type ClientReq t = Manager -> BaseUrl -> ExceptT ServantError IO t

-- | Define the client by pattern matching.
nixCacheInfo :: ClientReq NixCacheInfo
narInfo :: NarInfoReq -> ClientReq NarInfo
nar :: NarReq -> ClientReq Nar
queryPaths :: Vector FilePath -> ClientReq (HashMap FilePath Bool)
nixCacheInfo
  :<|> narInfo
  :<|> nar
  :<|> queryPaths = client (Proxy :: Proxy NixCacheAPI)

-- | Base URL of the nixos cache.
nixosCacheUrl :: BaseUrl
nixosCacheUrl = BaseUrl {
  baseUrlScheme = Https,
  baseUrlHost = "cache.nixos.org",
  baseUrlPort = 443,
  baseUrlPath = ""
  }

-- | A dependency tree, represented as a mapping from a store path to
-- its set of (immediate, not transitive) dependent paths.
newtype PathTree = PathTree (HashMap StorePath [StorePath])
  deriving (Show, Eq, Generic, Monoid)

-- | This function is not in all versions of the directory package, so
-- we copy/paste the definition here.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

-- | Write a path tree to the cache.
-- Iterates through keys of the path tree, for each one creates a
-- directory for the base path of the store path, and touches files
-- corresponding to paths of its dependencies.
-- So for example, if /nix/store/xyz-foo depends on /nix/store/{a,b,c},
-- then we will create
--   cacheLocation/xyz-foo/a
--   cacheLocation/xyz-foo/b
--   cacheLocation/xyz-foo/c
writeCache :: FilePath -- ^ Path to the cache.
           -> PathTree -- ^ Stuff to serialize.
           -> IO ()
writeCache cacheLocation (PathTree tree) = do
  -- mkdir -p cacheLocation
  createDirectoryIfMissing True cacheLocation
  forM_ (H.toList tree) $ \(path, refs) -> do
    let pathDir = cacheLocation </> spToPath path
    doesDirectoryExist pathDir >>= \case
      True -> pure ()
      False -> do
        -- Build the cache in a temporary directory, and then move it
        -- to the actual location. This prevents the creation of a
        -- partial directory in the case of a crash.
        tempDir <- readCreateProcess (shell "mktemp -d") ""
        forM_ refs $ \ref -> do
          writeFile (tempDir </> spToPath ref) ("" :: String)
        renameDirectory tempDir pathDir

-- | Read a cache into memory.
readCache :: FilePath -> IO PathTree
readCache cacheLocation = doesDirectoryExist cacheLocation >>= \case
  False -> pure mempty
  True -> do
    paths <- listDirectory cacheLocation
    tuples <- forM paths $ \path -> do
      bpath <- ioParseStorePath $ pack path
      deps <- do
        depfiles <- listDirectory (cacheLocation </> path)
        forM depfiles $ \path -> do
          ioParseStorePath $ pack path
      pure (bpath, deps)
    pure $ PathTree $ H.fromList tuples

-- | Configuration of the nix client.
data NixClientConfig = NixClientConfig {
  nccStoreDir :: NixStoreDir,
  -- ^ Location of the nix store.
  nccCacheLocation :: FilePath
  -- ^ Location of the nix client path cache.
  } deriving (Show, Generic)

-- | State for the nix client monad.
data NixClientState = NixClientState {
  ncsPathTree :: PathTree,
  -- ^ Computed store path dependency tree.
  ncsSentPaths :: HashSet StorePath
  -- ^ Paths that have already been sent to the nix store.
  } deriving (Show, Generic)

-- | Nix client monad.
type NixClient = ReaderT (NixClientConfig, MVar NixClientState) IO

-- | Run the nix client monad.
runNixClient :: NixClient a -> IO a
runNixClient action = do
  storeDir <- getNixStoreDir
  home <- getEnv "HOME"
  let cfg = NixClientConfig {
        nccStoreDir = storeDir,
        nccCacheLocation = home </> ".nix-path-cache"
        }
  pathTree <- readCache $ nccCacheLocation cfg
  let state = NixClientState { ncsPathTree = pathTree, ncsSentPaths = mempty }
  stateMVar <- newMVar state
  result <- runReaderT action (cfg, stateMVar)
  -- After we're done, read the new state from the MVar, and write it
  -- to the cache.
  state' <- readMVar stateMVar
  writeCache (nccCacheLocation cfg) (ncsPathTree state')
  pure result

-- | Get the references of an object by asking the nix-store. This
-- information is cached by the caller of this function.
getReferences' :: StorePath -> NixClient [StorePath]
getReferences' spath = do
  storeDir <- asks (nccStoreDir . fst)
  let cmd = "nix-store --query --references " <> spToFull storeDir spath
  result <- liftIO $ pack <$> readCreateProcess (shell $ unpack cmd) ""
  forM (splitWS result) $ \line -> case parseFullStorePath line of
    Left err -> error err
    Right (_, sp) -> pure sp

-- | Get references of a path, reading from and writing to a cache.
getReferences :: StorePath -> NixClient [StorePath]
getReferences spath = do
  mv <- asks snd
  modifyMVar mv $ \s -> do
    let PathTree t = ncsPathTree s
    case lookup spath t of
      Just refs -> pure (s, refs)
      Nothing -> do
        refs <- getReferences' spath
        pure (s {ncsPathTree = PathTree $ H.insert spath refs t}, refs)

-- | Send a path and its full dependency set to a binary cache.
sendClosure :: StorePath -> NixClient ()
sendClosure spath = do
  mv <- asks snd
  elem spath <$> ncsSentPaths <$> readMVar mv >>= \case
    True -> do
      -- Already sent, nothing more to do
      pure ()
    False -> do
      -- Not sent yet.
      refs <- getReferences spath
      -- Concurrently send parent paths, filtering out self-references.
      mapConcurrently sendClosure $ filter (/= spath) refs
      -- Once parents are sent, send the path itself.
      sendPath spath
      modifyMVar_ mv $ \s ->
        pure s {ncsSentPaths = HS.insert spath $ ncsSentPaths s}
  where
    -- | TODO (obvi): actually implement this function
    sendPath p = putStrLn $ "Sending " <> pack (spToPath p)
