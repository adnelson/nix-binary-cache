module Nix.Derivation.Tests where

import ClassyPrelude
import System.Directory (getDirectoryContents)
import System.Environment (getEnv)

import Nix.Derivation

parseAllDerivs :: IO ()
parseAllDerivs = do
  nixStore <- getEnv "NIX_STORE"
  let isDeriv path = ".drv" `isSuffixOf` path
  derivs <- filter isDeriv <$> getDirectoryContents nixStore
  forM_ derivs $ \path ->
    parseDerivFile (nixStore </> path) >>= \case
      Left err -> error $ "In file " <> path <> ": " <> err
      _ -> return ()
