module Main where

import ClassyPrelude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec (hspec)
import Control.Monad.Trans.Except (runExceptT)
import Servant.Client

import Nix.Cache.Client
import Nix.Cache.Types
import qualified Nix.Cache.Types.Tests as TypesTests

-- | An example store prefix that the nixos binary cache has (at present).
exPrefix :: StorePrefix
exPrefix = StorePrefix "8nm7vp5zw2lq3y2bvmq4yjnn2qgcm9rq"

-- | An example nar req that the nixos cache has (at present).
exNarReq :: NarReq
exNarReq = NarReq hash NarXzip
  where hash = "11wzap2jfs9pfp4qxff2rqw1bq1cxwd297rf55sf3hq2hb0prwkv"

-- | This function for easy testing.
run :: BaseUrl -> ClientReq a -> IO a
run baseUrl req = do
  manager <- newManager $ case baseUrlScheme baseUrl of
    Https -> tlsManagerSettings
    _ -> defaultManagerSettings
  runExceptT (req manager baseUrl) >>= \case
    Left err -> error $ show err
    Right x -> return x

-- | Make a request against the nixos cache.
runNixos :: ClientReq a -> IO a
runNixos = run nixosCacheUrl

main :: IO ()
main = hspec $ do
  TypesTests.nixCacheInfoSpec
  TypesTests.kvMapSpec
  TypesTests.fileHashSpec
