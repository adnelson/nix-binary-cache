-- | Nix store archives.
module Nix.Nar (
  Nar, -- Abstract
  getNar, getNixBinDir
  ) where

import ClassyPrelude
import qualified Data.ByteString as B
import System.Exit (ExitCode(..))
import Servant (MimeUnrender(..), OctetStream, ToHttpApiData(..),
                MimeRender(..))
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

-- | Get the nix binary directory path, e.g. where `nix-store` lives.
getNixBinDir :: IO FilePath
getNixBinDir = lookupEnv "NIX_BIN_DIR" >>= \case
  Just dir -> doesFileExist (dir </> "nix-store") >>= \case
    True -> pure dir
    False -> findit
  Nothing -> findit
  where
    whichNixStore = shell "which nix-store"
    findit = takeDirectory <$> readCreateProcess whichNixStore ""

-- | Ask nix for an archive of a store object.
getNar :: FilePath -> NixStoreDir -> StorePath -> IO Nar
getNar nixBin nsdir spath = do
  let nix_store = nixBin </> "nix-store"
      args = ["--export", spToFull nsdir spath]
      cmd = nix_store <> intercalate " " args
  readProcessWithExitCode nix_store args "" >>= \case
    (ExitSuccess, stdout, _) -> pure $ Nar stdout
    (ExitFailure code, _, _) -> error $ cmd <> " failed with " <> show code

-- instance ToHttpApiData Nar where
--   toUrlPiece (
instance MimeRender OctetStream Nar where
  mimeRender _ (Nar bytes) = fromChunks [bytes]
