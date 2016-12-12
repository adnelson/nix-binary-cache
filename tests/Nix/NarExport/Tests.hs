-- | WIP: tests for narexport
module Nix.NarExport.Tests where

import ClassyPrelude
import Test.Hspec

import Nix.Bin
import Nix.StorePath
import Nix.Nar
import Nix.NarExport
import Nix.Derivation.Tests (createRandomDeriv)

narExportSpec :: Spec
narExportSpec = describe "NarExport" $ do
  it "should create an export" $ do
    binDir <- getNixBinDir
    -- Create a random derivation so we have something to play with
    (sdir, dpath, _) <- createRandomDeriv
    nar <- getNar binDir sdir dpath
    export <- getExport binDir sdir dpath
    neNar export `shouldBe` nar
  it "should serialize the same as nix-store does" $ do
    binDir <- getNixBinDir
    -- Create a random derivation so we have something to play with
    (sdir, dpath, _) <- createRandomDeriv
    export <- getExport binDir sdir dpath
    nsExport <- nixCmd binDir "store" ["--export", spToFull sdir dpath]
    toStrict (narExportToBytestring export) `shouldBe` nsExport
