module Main where

import ClassyPrelude
import Test.Hspec (hspec)


import qualified Nix.Cache.Types.Tests as TypesTests
import qualified Data.KVMap.Tests as KVMapTests

import Nix.StorePath
import Nix.Cache.Client
import Nix.Derivation
import Nix.Derivation.Types

-- Define some things purely to suppress redundancy warnings
type FOO1 = StorePath
type FOO2 = NixCacheAuth
type FOO3 = Derivation
foo1 :: NixStoreDir -> Derivation -> IO PathSet
foo1 = derivInputs

main :: IO ()
main = hspec $ do
  TypesTests.nixCacheInfoSpec
  KVMapTests.kvMapSpec
  TypesTests.fileHashSpec
