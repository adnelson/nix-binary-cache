-- | Types relating to a nix binary cache.
module Nix.Cache.Types where

import ClassyPrelude
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Servant.API ((:<|>), (:>), Get, Capture)
import Data.Attoparsec.ByteString.Char8 (char, notChar, space, endOfLine,
                                         many1)
import Data.Attoparsec.ByteString.Lazy (Result(..), Parser, parse)
import Data.Aeson (ToJSON, FromJSON)
import Servant (MimeUnrender(..), OctetStream, ToHttpApiData(..))

-- | Some nix cache information comes in a line-separated "Key: Value"
-- format. Here we represent that as a map.
newtype KVMap = KVMap (HashMap Text Text)
  deriving (Show, Eq, Generic)

-- | Class for things which can be represented in KVMaps.
class FromKVMap t where
  fromKVMap :: KVMap -> Either String t

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

-- | A representation of a sha256 hash. This is encoded as a string in
-- the form "sha256:<hash>". The <hash> part might be encoded in hex
-- or in base32.
newtype FileHash = Sha256Hash Text
  deriving (Show, Eq, Generic)

fileHashFromText :: Text -> Either String FileHash
fileHashFromText = undefined

-- | Nix archive info.
data NarInfo = NarInfo {
  storePath :: FilePath, -- ^ Path of the store object.
  narHash :: FileHash, -- ^ Hash of the nix archive.
  narSize :: Int, -- ^ Size of the nix archive.
  fileSize :: Int, -- ^ Size of the uncompressed store object.
  fileHash :: FileHash, -- ^ Hash of the uncompressed store object.
  references :: [FilePath], -- ^ Other store objects this references.
  deriver :: Maybe FilePath -- ^ The derivation file for this object.
  } deriving (Show, Eq, Generic)

instance ToHttpApiData StorePrefix where
  toUrlPiece (StorePrefix prefix) = prefix <> ".narinfo"

instance FromKVMap NarInfo where
  fromKVMap (KVMap kvm) = do
    let lookupE key = case lookup key kvm of
          Nothing -> Left $ "No key " <> show key <> " was present."
          Just val -> return val
        parseNonNegInt txt = case readMay txt of
          Just n | n >= 0 -> Right n
          _ -> Left $ show txt <> " is not a non-negative integer"
        -- | Split a text on whitespace. Derp.
        splitWS = filter (/= "") . T.split (flip elem [' ', '\t', '\n', '\r'])

    storePath <- T.unpack <$> lookupE "StorePath"
    narHash <- lookupE "NarHash" >>= fileHashFromText
    narSize <- lookupE "NarSize" >>= parseNonNegInt
    fileSize <- lookupE "FileSize" >>= parseNonNegInt
    fileHash <- lookupE "FileHash" >>= fileHashFromText
    let references = case lookup "References" kvm of
          Nothing -> []
          Just refs -> map T.unpack $ splitWS refs
        deriver = Nothing
    return $ NarInfo storePath narHash narSize fileSize fileHash
               references deriver

instance MimeUnrender OctetStream NarInfo where
  mimeUnrender _ bstring = case parse parseKVMap bstring of
    Done _ kvmap -> fromKVMap kvmap
    Fail _ _ message -> Left message

-- | The nix cache API type.
type NixCacheAPI = "nix-cache-info" :> Get '[OctetStream] NixCacheInfo
              :<|> Capture "narinfo" StorePrefix :> Get '[OctetStream] NarInfo


-- | KVMaps can be parsed from text.
parseKVMap :: Parser KVMap
parseKVMap = do
  many $ endOfLine <|> (space >> return ())
  keysVals <- many $ do
    key <- many1 $ notChar ':'
    char ':' >> many space
    val <- many1 $ notChar '\n'
    many $ endOfLine <|> (space >> return ())
    return (T.pack key, T.pack val)
  return $ KVMap $ H.fromList keysVals
