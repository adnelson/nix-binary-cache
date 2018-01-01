-- | Nix store archives.
module Nix.Nar (
  module Nix.Nar.Types,
  module Nix.Nar.Serialization,
  getNar, getNarExport
  ) where

import ClassyPrelude
import Nix.Nar.Types
import Nix.Nar.Serialization
import Nix.Bin (NixBinDir, nixCmd)
import Nix.StorePath (StorePath, NixStoreDir, spToFull)

-- | Ask nix for an archive of a store object.
getNar :: NixBinDir -> NixStoreDir -> StorePath -> IO Nar
getNar nixBin nsdir spath = do
  let path = spToFull nsdir spath
  narBytes <- nixCmd nixBin "store" ["--dump", path]
  case runGet_ (fromStrict narBytes) of
    Right nar -> pure nar
    Left err -> error $ concat ["In file " <> show path <> ":\n", err]


-- | Ask nix for an export of a store object.
getNarExport :: NixBinDir -> NixStoreDir -> StorePath -> IO NarExport
getNarExport nixBin nsdir spath = do
  let path = spToFull nsdir spath
  narBytes <- nixCmd nixBin "store" ["--export", path]
  case runGet_ (fromStrict narBytes) of
    Right nar -> pure nar
    Left err -> error $ concat ["In file " <> show path <> ":\n", err]
