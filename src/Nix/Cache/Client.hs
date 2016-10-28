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
-- its set of dependent paths.
newtype PathTree = PathTree (HashMap StorePath (HashSet StorePath))

-- | Build a dependency tree given some starting store path.
buildTree :: StorePath -> IO PathTree
buildTree = undefined

-- | Given a store path, fetch all of the NARs of the path's
-- dependencies which are available from a cache, and put them in the
-- nix store.
fetchTree :: BaseUrl -> StorePath -> IO ()
fetchTree baseUrl storePath = undefined
