-- | Nix archives (Nar) and exports (NarExport)
module Nix.Nar.Types where

import ClassyPrelude hiding (take, try, Builder)
import qualified Data.ByteString as B

import Nix.StorePath (NixStoreDir, StorePath, PathSet)

-- | An archived nix store object.
newtype Nar = Nar NarElement deriving (Eq)

data IsExecutable = Executable | NotExecutable
  deriving (Show, Eq, Generic)

-- | An archived nix store object.
data NarElement
  = NarDirectory (HashMap B.ByteString NarElement)
  | NarFile IsExecutable B.ByteString
  | NarSymLink B.ByteString
  deriving (Show, Eq, Generic)

-- | Metadata associated with a NAR.
data NarMetadata = NarMetadata {
  nmStoreDirectory :: NixStoreDir,
  nmStorePath :: StorePath,
  nmReferences :: PathSet,
  nmDeriver :: Maybe StorePath,
  nmSignature :: Maybe Signature
  } deriving (Show, Eq, Generic)

newtype KeyName = KeyName Text
  deriving (Show, Eq, Generic, Hashable, IsString)

data Signature = Signature !KeyName !ByteString
  deriving (Show, Eq, Generic)

signatureToBytes :: Signature -> ByteString
signatureToBytes (Signature (KeyName key) sig) = encodeUtf8 key <> ":" <> sig

parseSignature :: ByteString -> Either String Signature
parseSignature bs = do
  let sep = fromIntegral $ fromEnum ':'
  case B.split sep bs of
    [key, bytes] -> Right (Signature (KeyName $ decodeUtf8 key) bytes)
    _ -> Left $ "Couldn't parse signature " <> show bs

-- | An exported nix archive
data NarExport = NarExport {neNar :: Nar, neMetadata :: NarMetadata}
  deriving (Eq, Generic, Show)

-- | Make a custom show instance so that we don't dump binary data to screen.
instance Show Nar where show _ = "Nix archive"
