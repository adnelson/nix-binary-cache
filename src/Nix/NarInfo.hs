-- | Haskell representation of NAR information.
module Nix.NarInfo where

import ClassyPrelude
import Data.KVMap
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import qualified Data.Text as T

import Servant (MimeUnrender(..), ToHttpApiData(..), OctetStream)

import Nix.FileHash (FileHash(..), fileHashFromText)

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
    let splitWS = filter (/= "") . T.split (flip elem [' ', '\t', '\n', '\r'])
        references = case lookup "References" kvm of
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
