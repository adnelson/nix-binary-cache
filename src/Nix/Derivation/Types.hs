module Nix.Derivation.Types where

import ClassyPrelude

import Nix.StorePath
import qualified Data.HashMap.Strict as H
import Nix.FileHash

-- | The main derivation type. This represents all of the information
-- that is needed to construct a particular store object; the store
-- object(s) that will be built are listed in the `derivOutputs` field.
data Derivation = Derivation {
  derivOutputs :: HashMap OutputName (StorePath, Maybe FileHash),
  -- ^ Outputs the derivation is expected to produce and what they're
  -- called. Those outputs might have known hashes (fixed-output
  -- derivations); if so include those.
  derivInputDerivations :: HashMap StorePath [OutputName],
  -- ^ Derivations this derivation needs to have as inputs, and
  -- outputs of those derivations.
  derivInputPaths :: [StorePath],
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

newtype OutputName = OutputName {outputName::Text}
  deriving (Show, Eq, Ord, Hashable, IsString)

-- | Given a derivation and an output name, return the path that the
-- output will correspond to.
--
-- For example, a derivation might be listed as having two outputs, "out"
-- and "dev". Once that derivation is built (successfully), two paths
-- will have been added to the nix store, one for each of the outputs.
-- This function given the output name will produce the corresponding
-- path that is expected to be built.
--
-- If the derivation doesn't provide the given output, an error is returned.
lookupOutput :: Derivation -> OutputName -> Maybe StorePath
lookupOutput Derivation{..} oname = fst <$> H.lookup oname derivOutputs
