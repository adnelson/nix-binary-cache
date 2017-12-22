-- | Nix store archives.
module Nix.Nar (
  module Nix.Nar.Types,
  module Nix.Nar.Serialization,
  getNar
  ) where

import ClassyPrelude
import Data.Binary (get)
import Data.Binary.Get (runGetOrFail, Get)
import Nix.Nar.Types
import Nix.Nar.Serialization
import Nix.Bin (NixBinDir, nixCmd)
import Nix.StorePath (StorePath, NixStoreDir, spToFull)

-- | Ask nix for an archive of a store object.
getNar :: NixBinDir -> NixStoreDir -> StorePath -> IO Nar
getNar nixBin nsdir spath = do
  let path = spToFull nsdir spath
  narBytes <- nixCmd nixBin "store" ["--dump", path]
  case runGetOrFail (get :: Get Nar) (fromStrict narBytes) of
    Right (_, _, nar) -> pure nar
    Left (_, c, e) -> error $
      concat ["In file " <> show path <> ":\n",
              "Character " <> show c <> ": " <> e]
