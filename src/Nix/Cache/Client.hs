module Nix.Cache.Client where

import Data.Default (Default(..))
import Servant.Client (BaseUrl(..), client, ServantError, Scheme(..))
import Network.HTTP.Client (Manager)
import Control.Monad.Trans.Except (ExceptT)
import Servant
import System.Process (readCreateProcess, shell)
import System.Directory (createDirectoryIfMissing, createDirectory,
                         getDirectoryContents,
                         doesDirectoryExist)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
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

instance Default PathTree where
  def = mempty

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
      True -> return ()
      False -> do
        createDirectory pathDir
        forM_ refs $ \ref -> do
          writeFile (pathDir </> spToPath ref) ("" :: String)

-- | Read a cache into memory.
readCache :: FilePath -> IO PathTree
readCache cacheLocation = doesDirectoryExist cacheLocation >>= \case
  False -> return mempty
  True -> do
    paths <- listDirectory cacheLocation
    tuples <- forM paths $ \path -> do
      bpath <- ioParseStorePath $ pack path
      deps <- do
        depfiles <- listDirectory (cacheLocation </> path)
        forM depfiles $ \path -> do
          ioParseStorePath $ pack path
      return (bpath, deps)
    return $ PathTree $ H.fromList tuples

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
  } deriving (Show, Generic)

-- | Nix client monad.
type NixClient = ReaderT NixClientConfig (StateT NixClientState IO)

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
  let state = NixClientState {
        ncsPathTree = pathTree,
        ncsSentPaths = mempty
        }
  (result, state') <- runStateT (runReaderT action cfg) state
  writeCache (nccCacheLocation cfg) (ncsPathTree state')
  return result

-- | Get references of a path, reading from a cache.
getReferences :: StorePath -> NixClient [StorePath]
getReferences spath = lookup_ spath <$> gets ncsPathTree >>= \case
  Just refs -> return refs
  Nothing -> do
    refs <- getReferences' spath
    modify $ \s -> s {ncsPathTree = insert_ spath refs (ncsPathTree s)}
    return refs
  where
    lookup_ path (PathTree t) = lookup path t
    insert_ path refs (PathTree t) = PathTree $ H.insert path refs t
    -- | Get the references of an object. Looks in and updates a global
    -- cache, since references are static information.
    getReferences' :: StorePath -> NixClient [StorePath]
    getReferences' spath = do
      storeDir <- asks nccStoreDir
      let cmd = "nix-store --query --references " <> spToFull storeDir spath
      result <- liftIO $ pack <$> readCreateProcess (shell $ unpack cmd) ""
      forM (splitWS result) $ \line -> case snd <$> parseFullStorePath line of
        Left err -> error err
        Right sbp -> return sbp

-- -- | Get the full dependency tree given some starting store path.
-- buildTree :: StorePath -> NixClient ()
-- buildTree spath = mapM_ buildTree =<< getReferences spath

-- | Send a path and its full dependency set to a binary cache.
sendClosure :: StorePath -> NixClient ()
sendClosure spath = elem spath <$> gets ncsSentPaths >>= \case
  True -> do
    -- Already sent, nothing more to do
    return ()
  False -> do
    -- Not sent yet. Send any parent paths, then send the path itself.
    refs <- getReferences spath
    -- Filter out self-referential paths
    mapM sendClosure $ filter (/= spath) refs
    sendPath spath
    modify $ \s -> s {ncsSentPaths = HS.insert spath (ncsSentPaths s)}
  where
    sendPath p = putStrLn $ "Sending " <> pack (spToPath p)

-- | Given a store path, fetch all of the NARs of the path's
-- dependencies which are available from a cache, and put them in the
-- nix store.
-- fetchTree :: BaseUrl -> StorePath -> IO ()
-- fetchTree = undefined
