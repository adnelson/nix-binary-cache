module Data.KVMap.Tests where

import ClassyPrelude hiding (ByteString)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.HashMap.Strict as H
import Data.Attoparsec.ByteString.Lazy

import Data.KVMap

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
