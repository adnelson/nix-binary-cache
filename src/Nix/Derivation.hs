-- | Nix derivations.
module Nix.Derivation where

import ClassyPrelude

newtype StorePath = StorePath FilePath
  deriving (Show, Eq, Generic)

data Derivation = Derivation {
  derivOutputs :: HashMap Text StorePath,
  -- ^ Outputs the derivation is expected to produce and what they're called.
  derivInputDerivations :: HashMap StorePath [Text],
  -- ^ Derivations this derivation needs to have as inputs, and
  -- outputs of those derivations.
  derivInputPaths :: [StorePath],
  -- ^ Non-derivation inputs the derivation needs in order to build.
  derivBuilder :: StorePath,
  -- ^ Path to the executable to build the derivation.
  derivArgs :: [Text],
  -- ^ Arguments to the builder.
  derivEnv :: HashMap Text Text,
  -- ^ Environment to run the builder in.
  derivSystem :: Text
  -- ^ System the derivation is to be built on.
  } deriving (Show, Eq, Generic)
