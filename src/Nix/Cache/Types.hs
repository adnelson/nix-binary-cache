{-# LANGUAGE UndecidableInstances #-}
-- | Types relating to a nix binary cache.
module Nix.Cache.Types where

import qualified Data.Text as T
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Data.Aeson (ToJSON, FromJSON)
import Servant (MimeUnrender(..), OctetStream, ToHttpApiData(..), Accept(..),
                Proxy(..), MimeRender(..))
import Network.HTTP.Media ((//))
import Codec.Compression.GZip (compress, decompress)

import Data.KVMap
import Nix.Cache.Common

-- | Type to represent the binary/octet-stream content type, which is
-- equivalent to application/octet-stream.
data BOctetStream

instance Accept BOctetStream where
  contentType _ = "binary" // "octet-stream"

-- | Convert OctetStream instances to BOctetStream instances. This is
-- why we need UndecidableInstances above.
instance MimeUnrender OctetStream t =>
         MimeUnrender BOctetStream t where
  mimeUnrender _ = mimeUnrender (Proxy :: Proxy OctetStream)


-- | Represents binary data compressed with gzip.
data GZipped

-- | Content type for gzipped data.
instance Accept GZipped where
  contentType _ = "application" // "x-gzip"

-- | Anything which can be put in an octet stream can be put in gzip.
instance MimeUnrender OctetStream t => MimeUnrender GZipped t where
  mimeUnrender _ = mimeUnrender (Proxy :: Proxy OctetStream) . decompress

instance MimeRender OctetStream t => MimeRender GZipped t where
  mimeRender _ obj = compress $ mimeRender (Proxy :: Proxy OctetStream) obj

-- | Information about a nix binary cache. This information is served
-- on the /nix-cache-info route.
data NixCacheInfo = NixCacheInfo {
  storeDir :: FilePath,
  -- ^ On-disk location of the nix store.
  wantMassQuery :: Bool,
  -- ^ Not sure what this does.
  priority :: Maybe Int
  -- ^ Also not sure what this means.
  } deriving (Show, Eq, Generic)

instance ToJSON NixCacheInfo
instance FromJSON NixCacheInfo

instance FromKVMap NixCacheInfo where
  fromKVMap (KVMap kvm) = case lookup "StoreDir" kvm of
    Nothing -> Left "No StoreDir key defined."
    Just sdir -> return $ NixCacheInfo {
      storeDir = T.unpack sdir,
      wantMassQuery = lookup "WantMassQuery" kvm == Just "1",
      priority = lookup "Priority" kvm >>= readMay
      }

-- | To parse something from an octet stream, first parse the
-- stream as a KVMap and then attempt to translate it.
instance MimeUnrender OctetStream NixCacheInfo where
  mimeUnrender _ bstring = case parse parseKVMap bstring of
    Done _ kvmap -> fromKVMap kvmap
    Fail _ _ message -> Left message

-- | The 32-character prefix of an object in the nix store.
newtype StorePrefix = StorePrefix Text
  deriving (Show, Eq, Generic)

-- | Requesting information about a nix archive, by providing its store prefix.
newtype NarInfoReq = NarInfoReq StorePrefix

-- | Store prefixes are used to request NAR information.
instance ToHttpApiData NarInfoReq where
  toUrlPiece (NarInfoReq (StorePrefix prefix)) = prefix <> ".narinfo"
