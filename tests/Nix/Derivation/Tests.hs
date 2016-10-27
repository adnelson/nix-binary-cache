module Nix.Derivation.Tests where

import System.Directory (listDirectory)
import System.Environment (getEnv)

import Nix.Derivation

parseAllDerivs :: IO ()
parseAllDerivs = do
  nixStore <- getEnv "NIX_STORE"
  storeObjs <- listDirectory nixStore
