module Nix.Cache.API where

import Servant

import Nix.Cache.Types


-- | The nix cache API type.
type NixCacheAPI = "nix-cache-info" :> Get '[OctetStream] NixCacheInfo
              :<|> Capture "narinfo" StorePrefix :> Get '[BOctetStream] NarInfo