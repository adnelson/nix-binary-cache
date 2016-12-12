-- | Nix store archives.
module Nix.Nar (
  Nar, -- Abstract
  getNar, getNar', narToBytestring, narFromBytestring
  ) where

import ClassyPrelude
import qualified Data.ByteString as B
import Servant (MimeUnrender(..), OctetStream, MimeRender(..))

import Nix.StorePath (NixStoreDir(..), NixBinDir(..), StorePath(..), spToFull,
                      nixStoreBS, getNixBinDir, getNixStoreDir)

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
getNar nixBin nsdir spath = Nar <$> nixStoreBS nixBin args where
  args = ["--dump", spToFull nsdir spath]

-- | Get an export using the default nix store and bin paths.
getNar' :: StorePath -> IO Nar
getNar' spath = do
  binDir <- getNixBinDir
  storeDir <- getNixStoreDir
  getNar binDir storeDir spath

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
