-- | Haskell abstraction for nix NAR exports.
module Nix.NarExport where

import ClassyPrelude
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8

import Nix.Nar (Nar, narToBytestring, getNar)
import Nix.Bin (NixBinDir, getNixBinDir, nixCmd)
import Nix.StorePath (StorePath, NixStoreDir, spToFull,
                      ioParseFullStorePath, getNixStoreDir)
import Data.ByteString.Builder (toLazyByteString, word64LE, byteString)

data NarExport = NarExport {
  neStoreDir :: NixStoreDir,
  -- ^ Path to the nix store.
  neStorePath :: StorePath,
  -- ^ Path of the contained store object.
  neNar :: Nar,
  -- ^ Archived store object.
  neReferences :: [StorePath],
  -- ^ References of the object.
  neDeriver :: Maybe StorePath
  -- ^ Path to the derivation that produced the store object.
  } deriving (Show, Eq, Generic)

-- | Load a nar export from disk. Note that we could do this by just
-- calling `nix-store --export`; however, since we know how to construct
-- an export one with more type safety, we can do this here.
getExport :: NixBinDir -> NixStoreDir -> StorePath -> IO NarExport
getExport nixBin storeDir spath = do
  let full = spToFull storeDir spath
  nar <- getNar nixBin storeDir spath
  -- Get references and deriver info from nix-store.
  refs <- nixCmd nixBin "store" ["--query", "--references", full]
  deriver <- nixCmd nixBin "store" ["--query", "--deriver", full] >>= \case
    "unknown-deriver\n" -> pure Nothing
    path -> Just . snd <$> ioParseFullStorePath path
  pure NarExport {neStoreDir = storeDir, neStorePath = spath,
                  neNar = nar, neReferences = refs, neDeriver = deriver}

-- | Get an export using the default nix store and bin paths.
getExport' :: StorePath -> IO NarExport
getExport' spath = do
  nixBin <- getNixBinDir
  storeDir <- getNixStoreDir
  getExport nixBin storeDir spath

-- | Get an export as a raw bytestring from the nix-store command.
getExportRaw :: NixBinDir -> NixStoreDir -> StorePath -> IO ByteString
getExportRaw nixBin storeDir spath = do
  nixCmd nixBin "store" ["--export", spToFull storeDir spath]

getExportRaw' :: StorePath -> IO ByteString
getExportRaw' spath = do
  nixBin <- getNixBinDir
  storeDir <- getNixStoreDir
  getExportRaw nixBin storeDir spath


-- | Magic number nix expects at the beginning of an export.
_EXPORT_MAGIC_1 :: ByteString
_EXPORT_MAGIC_1 = "\x01\x00\x00\x00\x00\x00\x00\x00"

-- Another magic 8-byte number that comes after the NAR.
_EXPORT_MAGIC_2 :: ByteString
_EXPORT_MAGIC_2 = "NIXE\x00\x00\x00\x00"

-- | Convert a NarExport to a ByteString.
--
-- Nix exports are a binary format. The logic of this function
-- serializes the NarExport, including:
--
-- * An 8-byte magic header, which nix-store reads when it imports.
-- * The bytes of the NAR itself.
-- * Another magic bytestring, which is 'NIXE' followed by four nulls.
-- * The path to the object in the nix store being imported.
-- * The number of references.
-- * The path of each reference.
-- * The deriver path, if known (else an empty string).
-- * 8 empty bytes, to indicate we're not including a signature.
-- * 8 empty bytes, for reasons unknown to me but needed by nix-store.
--
-- A note on string encoding:
--
-- Each string referenced above (e.g. paths) is represented by
-- first writing its length as an integer encoded in
-- little-endian 8 bytes, then the string itself, and then as
-- many null bytes as are needed to get to the nearest multiple
-- of 8 bytes. So for example, the string "hello" would be
-- represented as
--
--   "\x05\NUL\NUL\NUL\NUL\NUL\NUL\NULhello\NUL\NUL\NUL"
--
-- Note that there are three zeros following the "hello" text, in
-- order to pad it to eight bytes.
narExportToBytestring :: NarExport -> LB8.ByteString
narExportToBytestring NarExport{..} = toLazyByteString $ concat $ [
  -- Magic 8-byte number nix expects at the beginning of an export.
  byteString _EXPORT_MAGIC_1,
  -- Bytes of the nar itself.
  byteString $ narToBytestring neNar,
  -- Another magic 8-byte number that comes after the NAR.
  byteString _EXPORT_MAGIC_2,
  -- Store path of the object being archived.
  addStorePath neStorePath,
  -- Add the number of references.
  addInt $ length neReferences
  -- Add the paths of references, sorted lexicographically.
  ] <> map addStorePath (sort neReferences) <> [
  -- Add the deriver if it's known, otherwise an empty string.
  maybe (addString "") addStorePath neDeriver,
  -- Add 16 zeros: 8 to indicate no signature, and then another 8 to
  -- indicate the end of the export.
  byteString $ replicate 16 0
  ] where
    addInt i = word64LE (fromIntegral i)
    -- Implementing the string-encoding logic described above.
    addString str = concat [
        -- Add the length of a string, represented in 8 bytes.
        addInt (length str)
        -- Add the string.
      , byteString str
        -- Add padding if necessary, to make the total length a multiple of 8.
      , byteString $ let padding = 8 - (length str `mod` 8) in
        if (padding < 8) then replicate padding 0 else ""
      ]
    addStorePath sp = addString $! B8.pack $! spToFull neStoreDir sp
