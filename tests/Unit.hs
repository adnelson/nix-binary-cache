module Main where

import ClassyPrelude
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Request(..), ManagerSettings(..), newManager,
                            defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec (hspec)
import Control.Monad.Trans.Except (runExceptT)
import Servant.Client
import System.Environment (getEnv)

import Nix.Cache.Client
import Nix.Cache.Types
import qualified Nix.Cache.Types.Tests as TypesTests
import qualified Nix.Derivation.Tests as DerivTests

-- | An example store prefix that the nixos binary cache has (at present).
exPrefix :: StorePrefix
exPrefix = StorePrefix "8nm7vp5zw2lq3y2bvmq4yjnn2qgcm9rq"

-- | An example nar req that the nixos cache has (at present).
exNarReq :: NarReq
exNarReq = NarReq hash NarXzip
  where hash = "11wzap2jfs9pfp4qxff2rqw1bq1cxwd297rf55sf3hq2hb0prwkv"

-- | Username/password
type BasicAuth = (Text, Text)

-- | This function for easy testing.
run :: Maybe BasicAuth -> BaseUrl -> ClientReq a -> IO a
run mauth baseUrl req = do
  let managerSettings = case baseUrlScheme baseUrl of
        Https -> tlsManagerSettings
        _ -> defaultManagerSettings
      -- A request modifier function, which adds the username/password
      -- to the Authorization header.
      modifyReq req = return $ case mauth of
        Nothing -> req
        Just (username, password) -> do
          -- Encode the username:password in base64.
          let auth = username <> ":" <> password
          let authB64 = B64.encode $ T.encodeUtf8 auth
          req {
            requestHeaders = requestHeaders req `snoc`
              ("Authorization", "Basic " <> authB64)
          }

  manager <- newManager managerSettings {
    managerModifyRequest = modifyReq
    }
  runExceptT (req manager baseUrl) >>= \case
    Left err -> error $ show err
    Right x -> return x

runNs :: ClientReq a -> IO a
runNs req = do
  username <- T.pack <$> getEnv "NIX_BINARY_CACHE_USERNAME"
  password <- T.pack <$> getEnv "NIX_BINARY_CACHE_PASSWORD"
  let nsCacheUrl = BaseUrl {
    baseUrlScheme = Https,
    baseUrlHost = "nix.n-s.us",
    baseUrlPort = 443,
    baseUrlPath = ""
    }
  run (Just (username, password)) nsCacheUrl req

-- | Make a request against the nixos cache.
runNixos :: ClientReq a -> IO a
runNixos = run Nothing nixosCacheUrl

main :: IO ()
main = hspec $ do
  TypesTests.nixCacheInfoSpec
  TypesTests.kvMapSpec
  TypesTests.fileHashSpec
