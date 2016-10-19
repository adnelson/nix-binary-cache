-- | Types relating to a nix binary cache.
module Nix.Cache.Types where

import ClassyPrelude
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Attoparsec.ByteString.Char8 (char, notChar, space, endOfLine,
                                         many1)
import Data.Attoparsec.ByteString.Lazy (Result(..), Parser, parse, string)
import Data.Aeson (ToJSON, FromJSON)
import Servant (MimeUnrender(..), OctetStream)

-- | Some nix cache information comes in a line-separated "Key: Value"
-- format. Here we represent that as a map.
newtype KVMap = KVMap (HashMap Text Text)
  deriving (Show, Eq, Generic)

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

-- | Information about a nix binary cache. This information is served
-- on the /nix-cache-info route.
data NixCacheInfo = NixCacheInfo {
  storeDir :: FilePath,
  -- ^ On-disk location of the nix store.
  wantMassQuery :: Bool,
  -- ^ Not sure what this does
  priority :: Int
  -- ^ Also not sure what this means.
  } deriving (Show, Eq, Generic)

instance ToJSON NixCacheInfo
instance FromJSON NixCacheInfo

parseNixCacheInfo :: Parser NixCacheInfo
parseNixCacheInfo = do
  string "StoreDir: "
  storeDir_ <- many $ notChar '\n'
  return $ NixCacheInfo storeDir_ False 0

instance MimeUnrender OctetStream NixCacheInfo where
  -- | TODO: this is of course not accurate
  -- mimeUnrender :: Proxy OctetStream -> ByteString -> Either String NixCacheInfo
  mimeUnrender _ bstring = case parse parseNixCacheInfo bstring of
    Done _ info -> Right info
    Fail _ _ message -> Left message
