module Nix.Derivation.Types where

import ClassyPrelude

import Nix.StorePath
import Nix.FileHash

-- | The main derivation type. This represents all of the information
-- that is needed to construct a particular store object; the store
-- object(s) that will be built are listed in the `derivOutputs` field.
data Derivation = Derivation {
  derivOutputs :: HashMap Text (StorePath, Maybe FileHash),
  -- ^ Outputs the derivation is expected to produce and what they're
  -- called. Those outputs might have known hashes (fixed-output
  -- derivations); if so include those.
  derivInputDerivations :: HashMap StorePath [Text],
  -- ^ Derivations this derivation needs to have as inputs, and
  -- outputs of those derivations.
  derivInputPaths :: [FilePath],
  -- ^ Non-derivation inputs the derivation needs in order to build
  -- (paths that were copied from the file system to the store)
  derivSystem :: Text,
  -- ^ System the derivation is to be built on.
  derivBuilder :: FilePath,
  -- ^ Path to the executable to build the derivation.
  derivArgs :: [Text],
  -- ^ Arguments to the builder.
  derivEnv :: HashMap Text Text
  -- ^ Environment to run the builder in.
  } deriving (Show, Eq, Generic)
