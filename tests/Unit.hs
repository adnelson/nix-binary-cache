module Main where

import ClassyPrelude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec (hspec)
import Control.Monad.Trans.Except (runExceptT)
import Servant.Client

import Nix.Cache.Client
import Nix.Tests.Cache.Client

run :: BaseUrl -> IO ()
run baseUrl = do
  let managerSettings = case baseUrlScheme baseUrl of
        Https -> tlsManagerSettings
        _ -> defaultManagerSettings
  manager <- newManager managerSettings
  runExceptT (nixCacheInfo manager baseUrl) >>= \case
    Left err -> putStrLn $ "Error: " ++ tshow err
    Right info -> print info

main :: IO()
main = do
  hspec nixCacheInfoSpec
