module Nix.Cache.Client where

import ClassyPrelude
import Data.Proxy (Proxy(..))
import Servant.API ((:>), Get, JSON, OctetStream)
import Servant.Client (BaseUrl(..), client, ServantError, Scheme(..))
import Network.HTTP.Client (Manager)
import Control.Monad.Trans.Except (ExceptT)

import Nix.Cache.Types (NixCacheInfo(..))

type NixCacheAPI = "nix-cache-info" :> Get '[JSON, OctetStream] NixCacheInfo

-- | Define the client by pattern matching.
nixCacheInfo :: Manager -> BaseUrl -> ExceptT ServantError IO NixCacheInfo
nixCacheInfo = client (Proxy :: Proxy NixCacheAPI)

-- | Base URL of the nixos cache.
nixosCacheUrl :: BaseUrl
nixosCacheUrl = BaseUrl Http "cache.nixos.org" 80 ""
