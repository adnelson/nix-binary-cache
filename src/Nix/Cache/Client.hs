module Nix.Cache.Client where

import ClassyPrelude
import Servant.Client (BaseUrl(..), client, ServantError, Scheme(..))
import Network.HTTP.Client (Manager)
import Control.Monad.Trans.Except (ExceptT)
import Servant

import Nix.Cache.API
import Nix.Derivation (StorePath(..))
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
newtype PathTree = PathTree (HashMap StorePath (HashSet StorePath))

-- | Represents a path dependency cache. The cache is mutable, but
-- the set of dependencies of any particular path is fixed.
data PathCache = PathCache {
  pathCacheLocation :: FilePath
  } deriving (Show)

-- | Configuration of the nix client.
data NixClientConfig = NixClientConfig {
  nccCacheLocation :: FilePath
  } deriving (Show)

-- | Nix client type.
type NixClient a = ReaderT NixClientConfig IO a

-- | Get the references of an object. Looks in and updates a global
-- cache, since references are static information.
getReferences :: StorePath -> NixClient (HashSet StorePath)
getReferences path = undefined

-- | Get the full dependency tree given some starting store path.
buildTree :: StorePath -> PathTree -> IO PathTree
buildTree path ptree@(PathTree tree) = case lookup path tree of
  Just _ -> return ptree
  Nothing -> undefined

-- | Given a store path, fetch all of the NARs of the path's
-- dependencies which are available from a cache, and put them in the
-- nix store.
fetchTree :: BaseUrl -> StorePath -> IO ()
fetchTree baseUrl storePath = undefined
