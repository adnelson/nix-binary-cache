module Main where

import ClassyPrelude
import Test.Hspec (hspec)


import qualified Nix.Cache.Types.Tests as TypesTests
import qualified Data.KVMap.Tests as KVMapTests

import Nix.StorePath
import Nix.Cache.Client

main :: IO ()
main = hspec $ do
  TypesTests.nixCacheInfoSpec
  KVMapTests.kvMapSpec
  TypesTests.fileHashSpec
