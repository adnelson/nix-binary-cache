-- | Nix store archives.
module Nix.Nar (
  Nar, -- Abstract
  NixBinDir(..),
  getNar, getNixBinDir
  ) where

import ClassyPrelude
import qualified Data.ByteString as B
import System.Exit (ExitCode(..))
import Servant (MimeUnrender(..), OctetStream, MimeRender(..))
import System.Process (readCreateProcess, shell)
import System.Process.ByteString (readProcessWithExitCode)
import System.FilePath (takeDirectory)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

import Nix.StorePath (NixStoreDir(..), StorePath(..), spToFull)

-- | An archived nix store object.
newtype Nar = Nar ByteString
  deriving (Eq, Generic)

-- | Make a custom show instance so that we don't dump binary data to screen.
instance Show Nar where
  show (Nar bs) = "Nix archive, " <> show (B.length bs) <> " bytes"

-- | In the future, we could do validation on this.
instance MimeUnrender OctetStream Nar where
  mimeUnrender _ = return . Nar . toStrict

-- | Path to the directory containing nix binaries.
newtype NixBinDir = NixBinDir FilePath deriving (Show, Eq, IsString)

-- | Get the nix binary directory path, e.g. where `nix-store` lives.
getNixBinDir :: IO NixBinDir
getNixBinDir = lookupEnv "NIX_BIN_DIR" >>= \case
  Just dir -> doesFileExist (dir </> "nix-store") >>= \case
    True -> pure $ NixBinDir dir
    False -> findit
  Nothing -> findit
  where
    cmd = shell "which nix-store"
    findit = NixBinDir . takeDirectory <$> readCreateProcess cmd ""

-- | Ask nix for an archive of a store object.
getNar :: NixBinDir -> NixStoreDir -> StorePath -> IO Nar
getNar (NixBinDir nixBin) nsdir spath = do
  let nix_store = nixBin </> "nix-store"
      args = ["--export", spToFull nsdir spath]
  readProcessWithExitCode nix_store args "" >>= \case
    (ExitSuccess, stdout, _) -> pure $ Nar stdout
    (ExitFailure code, _, _) -> error $ cmd <> " failed with " <> show code
      where cmd = nix_store <> intercalate " " args

-- instance ToHttpApiData Nar where
--   toUrlPiece (
instance MimeRender OctetStream Nar where
  mimeRender _ (Nar bytes) = fromChunks [bytes]
