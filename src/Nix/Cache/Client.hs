module Nix.Cache.Client where

import Data.Default (Default(..))
import Servant.Client (BaseUrl(..), client, ServantError, Scheme(..))
import Network.HTTP.Client (Manager)
import Control.Monad.Trans.Except (ExceptT)
import Servant
import System.Process (readCreateProcess, shell)
import System.Directory (createDirectoryIfMissing,
                         getDirectoryContents,
                         doesDirectoryExist)
import System.Posix.Files (touchFile)
import qualified Data.HashMap.Strict as H

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
newtype PathTree = PathTree (HashMap StoreBasepath [StoreBasepath])
  deriving (Show, Eq, Generic, Monoid)

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
writeCache :: FilePath -- ^ Path to the nix store.
           -> FilePath -- ^ Path to the cache.
           -> PathTree -- ^ Stuff to serialize.
           -> IO ()
writeCache storeDir cacheLocation (PathTree tree) = do
  -- mkdir -p cacheLocation
  createDirectoryIfMissing True cacheLocation
  forM_ (H.toList tree) $ \(path, refs) -> do
    let pathDir = cacheLocation </> sbpRender path
    createDirectoryIfMissing False pathDir
    forM_ refs $ \ref -> do
      touchFile $ pathDir </> sbpRender ref

-- | Read a cache into memory.
readCache :: FilePath -> IO PathTree
readCache cacheLocation = doesDirectoryExist cacheLocation >>= \case
  False -> return mempty
  True -> do
    paths <- listDirectory cacheLocation
    tuples <- forM paths $ \path -> do
      bpath <- ioParseStoreBasepath $ pack path
      deps <- do
        depfiles <- listDirectory (cacheLocation </> path)
        forM depfiles $ \path -> do
          ioParseStoreBasepath $ pack path
      return (bpath, deps)
    return $ PathTree $ H.fromList tuples

-- | Represents a path dependency cache. The cache is mutable, but
-- the set of dependencies of any particular path is fixed.
data PathCache = PathCache {
  pathCacheLocation :: FilePath
  } deriving (Show)

-- | Configuration of the nix client.
data NixClientConfig = NixClientConfig {
  nccCacheLocation :: FilePath
  } deriving (Show, Generic)

instance Default NixClientConfig

-- | Nix client monad.
type NixClient = ReaderT NixClientConfig (ReaderT PathTree IO)

-- | Run the nix client monad.
runNixClient :: NixClientConfig -> NixClient a -> IO a
runNixClient cfg action = runReaderT (runReaderT action cfg) mempty

-- | Get references of a path, reading from a cache.
getReferencesCached :: StorePath -> NixClient [StorePath]
getReferencesCached = undefined

-- | Get the references of an object. Looks in and updates a global
-- cache, since references are static information.
getReferences :: StorePath -> NixClient [StorePath]
getReferences spath = do
  let cmd = "nix-store --query --references " <> spFullpath spath
  result <- liftIO $ pack <$> readCreateProcess (shell $ unpack cmd) ""
  forM (splitWS result) $ \line -> case parseStorePath line of
    Left err -> error err
    Right sp -> return sp

-- -- | Get the full dependency tree given some starting store path.
-- buildTree :: StorePath -> PathTree -> IO PathTree
-- buildTree path ptree@(PathTree tree) = case lookup path tree of
--   Just _ -> return ptree
--   Nothing -> undefined

-- | Given a store path, fetch all of the NARs of the path's
-- dependencies which are available from a cache, and put them in the
-- nix store.
fetchTree :: BaseUrl -> StorePath -> IO ()
fetchTree = undefined
