{-# LANGUAGE UndecidableInstances #-}
-- | Types relating to a nix binary cache.
module Nix.Cache.Types where

import qualified Data.Text as T
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Data.Aeson (ToJSON, FromJSON)
import Servant (MimeUnrender(..), OctetStream, ToHttpApiData(..), Accept(..),
                Proxy(..))
import Network.HTTP.Media ((//))

import Data.KVMap
import Nix.Cache.Common
import Nix.StorePath (StorePrefix(..))
import Nix.Derivation (FileHash(..), fileHashFromText)

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

-- | Requesting information about a nix archive, by providing its store prefix.
newtype NarInfoReq = NarInfoReq StorePrefix

-- | Store prefixes are used to request NAR information.
instance ToHttpApiData NarInfoReq where
  toUrlPiece (NarInfoReq (StorePrefix prefix)) = prefix <> ".narinfo"

-- | Nix archive info. This returns metadata about an object that the
-- binary cache can serve to a client.
data NarInfo = NarInfo {
  storePath :: FilePath, -- ^ Path of the store object.
  narHash :: FileHash, -- ^ Hash of the nix archive.
  narSize :: Int, -- ^ Size of the nix archive.
  fileSize :: Int, -- ^ Size of the uncompressed store object.
  fileHash :: FileHash, -- ^ Hash of the uncompressed store object.
  narReq :: NarRequest, -- ^ How to request this NAR.
  compression :: NarCompressionType, -- ^ How this NAR is compressed.
  references :: [FilePath], -- ^ Other store objects this references.
  deriver :: Maybe FilePath, -- ^ The derivation file for this object.
  sig :: Maybe Text -- Possible signature of the cache.
  } deriving (Show, Eq, Generic)

instance FromKVMap NarInfo where
  fromKVMap (KVMap kvm) = do
    let lookupE key = case lookup key kvm of
          Nothing -> Left $ "No key " <> show key <> " was present."
          Just val -> return val
        parseNonNegInt txt = case readMay txt of
          Just n | n >= 0 -> Right n
          _ -> Left $ show txt <> " is not a non-negative integer"
        parseNarRequest compType txt = do
          let suf = compTypeToExt compType
          case "nar/" `T.isPrefixOf` txt of
            False -> Left "Expected nar req to start with 'nar/'"
            True -> case suf `T.isSuffixOf` txt of
              False -> Left $ "Expected nar req to end with " <> show suf
              True -> do
                let narPath = T.drop 4 $ T.dropEnd (length suf) txt
                return $ NarRequest narPath compType

    storePath <- T.unpack <$> lookupE "StorePath"
    narHash <- lookupE "NarHash" >>= fileHashFromText
    narSize <- lookupE "NarSize" >>= parseNonNegInt
    fileSize <- lookupE "FileSize" >>= parseNonNegInt
    fileHash <- lookupE "FileHash" >>= fileHashFromText
    compression <- lookupE "Compression" >>= parseCompressionType
    narReq <-  lookupE "URL" >>= parseNarRequest compression
    let references = case lookup "References" kvm of
          Nothing -> []
          Just refs -> map T.unpack $ splitWS refs
        deriver = Nothing
        sig = lookup "Sig" kvm
    return $ NarInfo storePath narHash narSize fileSize fileHash
               narReq compression references deriver sig

instance MimeUnrender OctetStream NarInfo where
  mimeUnrender _ bstring = case parse parseKVMap bstring of
    Done _ kvmap -> fromKVMap kvmap
    Fail _ _ message -> Left message

-- | Types of compression supported for NAR archives.
data NarCompressionType = NarBzip2 | NarXzip
  deriving (Show, Eq, Generic)

-- | Convert a compression type into a filename extension.
compTypeToExt :: NarCompressionType -> Text
compTypeToExt NarBzip2 = ".nar.bz2"
compTypeToExt NarXzip = ".nar.xz"

-- | Convert a compression type string.
parseCompressionType :: Text -> Either String NarCompressionType
parseCompressionType = \case
  "xz" -> return NarXzip
  "xzip" -> return NarXzip
  "bz2" -> return NarBzip2
  "bzip2" -> return NarBzip2
  ctype -> Left (show ctype <> " is not a known compression type.")

-- | Request for a nix archive.
-- The first argument is some sort of key that the server provides (as
-- a response to the .narinfo route) for how to fetch the package. The
-- second argument is the compression type.
data NarRequest = NarRequest Text NarCompressionType
  deriving (Show, Eq, Generic)

-- | Store prefixes are used to request NAR information.
instance ToHttpApiData NarRequest where
  toUrlPiece (NarRequest key ctype) = key <> compTypeToExt ctype
