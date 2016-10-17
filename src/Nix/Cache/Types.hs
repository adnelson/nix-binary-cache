-- | Types relating to a nix binary cache.
module Nix.Cache.Types where

import ClassyPrelude
import Data.Attoparsec.ByteString () -- hiding (parse, Result(..))
import Data.Attoparsec.ByteString.Char8 (notChar)-- hiding (parse, Result(..))
import Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Servant ((:>), Get, OctetStream, Proxy(..), MimeUnrender(..))
import Servant.Client


-- | Information about a nix binary cache. This information is served
-- on the /nix-cache-info route.
data NixCacheInfo = NixCacheInfo {
  nciStoreDir :: FilePath,
  -- ^ On-disk location of the nix store.
  nciWantMassQuery :: Bool,
  -- ^ Not sure what this does
  nciPriority :: Int
  -- ^ Also not sure what this means.
  } deriving (Show, Eq, Generic)

instance ToJSON NixCacheInfo
instance FromJSON NixCacheInfo

instance MimeUnrender OctetStream NixCacheInfo where
  -- | TODO: this is of course not accurate
  -- mimeUnrender :: Proxy OctetStream -> ByteString -> Either String NixCacheInfo
  mimeUnrender _ bstring = case parse parser bstring of
    Done _ info -> Right info
    Fail _ _ message -> Left message
    where
      parser = do
        string "StoreDir: "
        storeDir <- many $ notChar '\n'
        return $ NixCacheInfo storeDir False 0


type NixCacheAPI = "nix-cache-info" :> Get '[OctetStream] NixCacheInfo

nixCacheInfo :: Client NixCacheAPI
nixCacheInfo = client (Proxy :: Proxy NixCacheAPI)

run :: IO ()
run = do
  let
    queries :: Manager -> BaseUrl -> ExceptT ServantError IO NixCacheInfo
    queries manager baseurl = client (Proxy :: Proxy NixCacheAPI)
  manager <- newManager defaultManagerSettings
  res <- runExceptT (queries manager (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " <> tshow err
    Right info -> print info
