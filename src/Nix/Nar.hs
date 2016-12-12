-- | Nix store archives.
module Nix.Nar (
  Nar, -- Abstract
  getNar, narToBytestring, narFromBytestring
  ) where

import ClassyPrelude
import qualified Data.ByteString as B
import System.Exit (ExitCode(..))
import Servant (MimeUnrender(..), OctetStream, MimeRender(..))
import System.Process.ByteString (readProcessWithExitCode)

import Nix.StorePath (NixStoreDir(..), NixBinDir(..), StorePath(..), spToFull)

-- | An archived nix store object.
newtype Nar = Nar ByteString
  deriving (Eq, Generic)

-- | Make a custom show instance so that we don't dump binary data to screen.
instance Show Nar where
  show (Nar bs) = "Nix archive, " <> show (B.length bs) <> " bytes"

-- | In the future, we could do validation on this.
instance MimeUnrender OctetStream Nar where
  mimeUnrender _ = return . Nar . toStrict

-- | Ask nix for an archive of a store object.
getNar :: NixBinDir -> NixStoreDir -> StorePath -> IO Nar
getNar (NixBinDir nixBin) nsdir spath = do
  let nix_store = nixBin </> "nix-store"
      args = ["--export", spToFull nsdir spath]
  readProcessWithExitCode nix_store args "" >>= \case
    (ExitSuccess, stdout, _) -> pure $ Nar stdout
    (ExitFailure code, _, _) -> error $ cmd <> " failed with " <> show code
      where cmd = nix_store <> intercalate " " args

-- | Convert a NAR to a bytestring.
narToBytestring :: Nar -> ByteString
narToBytestring (Nar bytes) = bytes

-- | Convert a bytestring to a NAR.
narFromBytestring :: ByteString -> Maybe Nar
narFromBytestring = Just . Nar

-- instance ToHttpApiData Nar where
--   toUrlPiece (
instance MimeRender OctetStream Nar where
  mimeRender _ (Nar bytes) = fromChunks [bytes]
