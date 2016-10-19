module Nix.Tests.Cache.Client where

import ClassyPrelude hiding (ByteString)
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as H
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Servant

import Nix.Cache.Types

kvMapSpec :: Spec
kvMapSpec = describe "KVMap" $ do
  describe "parsing" $ do
    let txt = "X: hey\nY: Yo!"
        kvmap = KVMap $ H.fromList [
          ("X", "hey"),
          ("Y", "Yo!")
          ]
    it "should parse a kv map" $ do
      case parse parseKVMap txt of
        Done _ kvmap' -> kvmap' `shouldBe` kvmap
        Fail _ _ message -> error message

nixCacheInfoSpec :: Spec
nixCacheInfoSpec = describe "nix-cache-info" $ do
  describe "parsing" $ do
    let txt = "StoreDir: /test/store/dir"
        info = NixCacheInfo {
          storeDir = "/test/store/dir",
          wantMassQuery = False,
          priority = 0
          }
        unrender :: ByteString -> Either String NixCacheInfo
        unrender = mimeUnrender (Proxy :: Proxy OctetStream)
    it "should parse the storedir" $ do
      unrender txt `shouldBe` Right info
    it "should not have a problem with newlines" $ do
      unrender (txt <> "\n") `shouldBe` Right info
    it "should fail if the storedir isn't there" $ do
      let bad = "StoreDerp: /test/store/derp"
      unrender bad `shouldSatisfy` isLeft
