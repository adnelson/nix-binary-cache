module Nix.Cache.API where

import ClassyPrelude (Vector, FilePath, HashMap, Bool)
import Servant
import Servant.HTML.Lucid (HTML)

import Nix.Nar (Nar, NarExport)
import Nix.StorePath (StorePath)
import Nix.Cache.Types (GZipped, NixCacheInfo)
import Nix.NarInfo (NarInfo, NarRequest, NarInfoReq)

-- | The nix cache API type.
type NixCacheAPI = "nix-cache-info" :> Get '[OctetStream] NixCacheInfo
              :<|> Capture "narinfo" NarInfoReq :> Get '[OctetStream] NarInfo
              :<|> "nar" :> Capture "nar" NarRequest :> Get '[OctetStream] Nar
              :<|> "query-paths" :> ReqBody '[JSON] (Vector FilePath)
                                 :> Get '[JSON] (HashMap FilePath Bool)
              :<|> "import-path" :> ReqBody '[GZipped] NarExport
                                 :> Post '[HTML, OctetStream] StorePath
