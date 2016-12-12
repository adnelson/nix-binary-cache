module Nix.Cache.API where

import ClassyPrelude (Vector, FilePath, HashMap, Bool)
import Servant
import Servant.HTML.Lucid (HTML)

import Nix.Nar
import Nix.StorePath
import Nix.Cache.Types
import Nix.NarInfo

-- | Due to AWS S3, we have to support a bogus 'binary/octet-stream'
-- content type. The `OStream` type thus represents something which is
-- either an `application/octet-stream` or a `binary/octet-stream`.
type OStream = '[OctetStream, BOctetStream]

-- | The nix cache API type.
type NixCacheAPI = "nix-cache-info" :> Get OStream NixCacheInfo
              :<|> Capture "narinfo" NarInfoReq :> Get OStream NarInfo
              :<|> "nar" :> Capture "nar" NarRequest :> Get OStream Nar
              :<|> "query-paths" :> ReqBody '[JSON] (Vector FilePath)
                                 :> Get '[JSON] (HashMap FilePath Bool)
              :<|> "import-path" :> ReqBody '[GZipped] Nar
                                 :> Post '[HTML, OctetStream] StorePath
