-- | Tests related to the base types that support the nix cache.
module Nix.Cache.Types.Tests where

import ClassyPrelude hiding (ByteString)
import Test.QuickCheck (Arbitrary(..), oneof, property, elements)
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as H
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Servant

import Nix.Cache.Types
import Nix.Derivation (FileHash(..), fileHashToText, fileHashFromText)

instance Arbitrary Text where
  arbitrary = fromString <$> arbitrary

instance Arbitrary FileHash where
  arbitrary = do
    let letters = ['a'..'f'] <> ['0'..'9']
        str = replicateM 32 $ elements letters
    oneof [Sha256Hash <$> str, Sha1Hash <$> str, Md5Hash <$> str,
           RecursiveHash <$> arbitrary]

fileHashSpec :: Spec
fileHashSpec = describe "file hashes" $ do
  it "should parse from a string" $ do
    property $ \hash -> do
      let strRep = fileHashToText hash
      fileHashFromText strRep `shouldBe` Right hash

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
          priority = Nothing
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
    it "should grab wantmassquery" $ do
      let txt = "StoreDir: /x\nWantMassQuery: 1"
          info = NixCacheInfo {
            storeDir = "/x",
            wantMassQuery = True,
            priority = Nothing
            }
      unrender txt `shouldBe` Right info
    it "should grab priority" $ do
      let txt = "Priority: 23\nStoreDir: /x\nWantMassQuery: 1"
          info = NixCacheInfo {
            storeDir = "/x",
            wantMassQuery = True,
            priority = Just 23
            }
      unrender txt `shouldBe` Right info
