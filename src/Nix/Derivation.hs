-- | Nix derivations.
module Nix.Derivation where

import ClassyPrelude
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS

import Nix.Derivation.Types
import Nix.Derivation.Parser
import Nix.StorePath

-- | Parse a derivation file. Assumes the file exists and parses correctly.
parseDerivFromPath :: NixStoreDir -> StorePath -> IO Derivation
parseDerivFromPath sdir spath = parseDerivFile (spToFull sdir spath) >>= \case
  Left err -> fail err
  Right deriv -> pure deriv

-- | Given a derivation, retrieve all of the paths it requires to
-- build. Note this is not the full closure, because it does not recur on
-- other derivations.
derivInputs :: NixStoreDir -> Derivation -> IO PathSet
derivInputs storeDir Derivation{..} = do
  -- Get the paths of all of the input derivations.
  inputsFromDerivs :: [StorePath] <- concat <$> do
    forM (H.toList derivInputDerivations) $ \(dpath, outNames) -> do
      deriv <- parseDerivFromPath storeDir dpath
      pure $ catMaybes $ lookupOutput deriv <$> outNames
  pure $ HS.fromList $ derivInputPaths <> inputsFromDerivs
