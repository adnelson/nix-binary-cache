-- | Nix store archives.
module Nix.Nar where

import ClassyPrelude
import qualified Data.ByteString as B
import System.Process hiding (readCreateProcess)
import System.Exit (ExitCode(..))
import Servant (MimeUnrender(..), OctetStream, ToHttpApiData(..),
                MimeRender(..))
                -- Accept(..),
                -- Proxy(..))

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

-- | Ask nix for an archive of a store object.
getNar :: NixStoreDir -> StorePath -> IO Nar
getNar nsdir spath = do
  let cmd = "nix-store --export " <> spToFull nsdir spath
      procSpec = (shell cmd) { std_out = CreatePipe }
  (_, Just stdout, _, handle) <- createProcess procSpec
  waitForProcess handle >>= \case
    ExitFailure code -> error $ cmd <> " failed with " <> show code
    ExitSuccess -> Nar <$> B.hGetContents stdout

-- instance ToHttpApiData Nar where
--   toUrlPiece (
instance MimeRender OctetStream Nar where
  mimeRender _ (Nar bytes) = fromChunks [bytes]
