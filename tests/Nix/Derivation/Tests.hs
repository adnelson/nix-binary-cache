module Nix.Derivation.Tests where

import ClassyPrelude
import qualified Data.Text as T
import Test.Hspec
import Test.RandomStrings (randomWord, randomASCII)
import qualified Data.HashMap.Strict as H

import Nix.Derivation
import Nix.StorePath
import Nix.Bin

-- | Create a minimal derivation in the nix store. Does not do
-- resource cleanup! These files should be tiny though.
createDeriv :: Text -> Text -> Text -> IO (NixStoreDir, StorePath, Derivation)
createDeriv name builder system= do
  let expr = concat ["derivation {name = ", show name, ";",
                     "builder = ", show builder, ";",
                     "system = ", show system, ";}"]
  (storeDir, path) <- nixCmd' "instantiate" ["-E", expr]
  deriv <- parseDerivFromPath storeDir path
  return (storeDir, path, deriv)

createRandomDeriv :: IO (NixStoreDir, StorePath, Derivation)
createRandomDeriv = do
  name <- T.pack <$> randomWord randomASCII 10
  builder <- T.pack <$> randomWord randomASCII 10
  system <- T.pack <$> randomWord randomASCII 10
  createDeriv name builder system

derivSpec :: Spec
derivSpec = describe "derivations" $ do
  it "should parse a derivation" $ do
    name <- T.pack <$> randomWord randomASCII 10
    builder <- T.pack <$> randomWord randomASCII 10
    system <- T.pack <$> randomWord randomASCII 10
    (_, _, deriv) <- createDeriv name builder system
    H.size (derivEnv deriv) `shouldBe` 4
    derivGetEnv "name" deriv `shouldBe` Just name
    derivGetEnv "builder" deriv `shouldBe` Just builder
    derivGetEnv "system" deriv `shouldBe` Just system
    let Just out = derivGetEnv "out" deriv
    outpath <- snd <$> ioParseFullStorePath out
    derivGetOut "out" deriv `shouldBe` Just (outpath, Nothing)



-- parseAllDerivs :: IO ()
-- parseAllDerivs = do
--   nixStore <- getEnv "NIX_STORE"
--   let isDeriv path = ".drv" `isSuffixOf` path
--   derivs <- filter isDeriv <$> getDirectoryContents nixStore
--   forM_ derivs $ \path ->
--     parseDerivFile (nixStore </> path) >>= \case
--       Left err -> error $ "In file " <> path <> ": " <> err
--      _ -> return ()
