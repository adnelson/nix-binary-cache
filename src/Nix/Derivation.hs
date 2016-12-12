-- | Nix derivations.
module Nix.Derivation (
  module Nix.Derivation.Types,
  parseDerivFromPath,
  parseDerivFromPath',
  parseDerivFromPrefix,
  derivInputs, derivGetEnv, derivGetOut
  ) where

import ClassyPrelude
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Nix.Derivation.Types
import Nix.Derivation.Parser
import Nix.StorePath
import Nix.FileHash

-- | Parse a derivation file. Assumes the file exists and parses correctly.
parseDerivFromPath :: NixStoreDir -> StorePath -> IO Derivation
parseDerivFromPath sdir spath = parseDerivFile (spToFull sdir spath) >>= \case
  Left err -> fail err
  Right deriv -> pure deriv

-- | Parse a derivation file from a storepath, using the NIX_STORE variable.
parseDerivFromPath' :: StorePath -> IO Derivation
parseDerivFromPath' p = getNixStoreDir >>= flip parseDerivFromPath p

-- | Parse a derivation file given its store prefix.
parseDerivFromPrefix :: Text -> IO Derivation
parseDerivFromPrefix prefix = parseDerivFromPath' =<< findSpByPrefix prefix

-- | Given a derivation, retrieve all of the derivation paths it
-- requires to build. Note this is not the full closure, because it
-- does not recur on other derivations. Also note that this does not
-- include fixturized filesystem paths; it only returns input paths
-- which are derivations. This is because any filesystem path which
-- appears in a derivation is known to already exist in the nix store.
derivInputs :: NixStoreDir -> Derivation -> IO PathSet
derivInputs storeDir Derivation{..} = HS.fromList . concat <$> do
  forM (H.toList derivInputDerivations) $ \(dpath, outNames) -> do
    deriv <- parseDerivFromPath storeDir dpath
    pure $ catMaybes $ lookupOutput deriv <$> outNames

derivGetEnv :: Text -> Derivation -> Maybe Text
derivGetEnv key = H.lookup key . derivEnv

derivGetOut :: OutputName -> Derivation -> Maybe (StorePath, Maybe FileHash)
derivGetOut outName = H.lookup outName . derivOutputs
