-- | Types relating to a nix binary cache.
module Nix.Cache.Types where

import ClassyPrelude
import Data.Attoparsec.ByteString.Char8 (notChar)-- hiding (parse, Result(..))
import Data.Attoparsec.ByteString.Lazy (Result(..), Parser, parse, string)
import Data.Aeson (ToJSON, FromJSON)
import Servant (MimeUnrender(..), OctetStream)

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
