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
    pure $ catMaybes $ flip map outNames $ \name -> do
      case lookupOutput deriv name of
        Left _ -> Nothing
        Right path -> Just path

derivGetEnv :: Text -> Derivation -> Maybe Text
derivGetEnv key = H.lookup key . derivEnv

derivGetOut :: OutputName -> Derivation -> Maybe (StorePath, Maybe FileHash)
derivGetOut outName = H.lookup outName . derivOutputs
