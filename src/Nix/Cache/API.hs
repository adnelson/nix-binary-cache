module Nix.Cache.API where

import ClassyPrelude (Vector, FilePath, HashMap, Bool)
import Servant

import Nix.Cache.Types

type OStream = '[OctetStream, BOctetStream]

-- | The nix cache API type.
type NixCacheAPI = "nix-cache-info" :> Get OStream NixCacheInfo
              :<|> Capture "narinfo" NarInfoReq :> Get OStream NarInfo
              :<|> "nar" :> Capture "nar" NarReq :> Get OStream Nar
              :<|> "query-paths" :> ReqBody '[JSON] (Vector FilePath)
                                 :> Get '[JSON] (HashMap FilePath Bool)
