-- | Haskell abstraction for nix NAR exports.
module Nix.NarExport where

import ClassyPrelude
import qualified Control.Monad.State.Strict as State
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Nix.Nar (Nar, narToBytestring, getNar)
import Nix.StorePath (StorePath, NixBinDir(..), NixStoreDir, spToFull,
                      ioParseFullStorePath, nixStoreText, nixStoreBS,
                      getNixBinDir, getNixStoreDir)
import Data.ByteString.Builder (toLazyByteString, word64LE)

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
  refs <- do
    paths <- T.lines <$> nixStoreText nixBin ["--query", "--references", full]
    forM paths $ \rawSpath -> do
      snd <$> ioParseFullStorePath rawSpath
  deriver <- nixStoreText nixBin ["--query", "--deriver", full] >>= \case
    "unknown-deriver\n" -> pure Nothing
    path -> Just . snd <$> ioParseFullStorePath path
  pure NarExport {neStoreDir = storeDir, neStorePath = spath,
                  neNar = nar, neReferences = refs, neDeriver = deriver}

getExport' :: StorePath -> IO NarExport
getExport' spath = do
  nixBin <- getNixBinDir
  storeDir <- getNixStoreDir
  getExport nixBin storeDir spath

-- | Get an export as a raw bytestring from the nix-store command.
getExportRaw :: NixBinDir -> NixStoreDir -> StorePath -> IO ByteString
getExportRaw nixBin storeDir spath = do
  nixStoreBS nixBin ["--export", spToFull storeDir spath]

getExportRaw' :: StorePath -> IO ByteString
getExportRaw' spath = do
  nixBin <- getNixBinDir
  storeDir <- getNixStoreDir
  getExportRaw nixBin storeDir spath


-- | Magic 8-byte number nix expects at the beginning of an export.
_EXPORT_MAGIC_1 :: ByteString
_EXPORT_MAGIC_1 = "\x01\x00\x00\x00\x00\x00\x00\x00"

-- Another magic 8-byte number that comes after the NAR.
_EXPORT_MAGIC_2 :: ByteString
_EXPORT_MAGIC_2 = "NIXE\x00\x00\x00\x00"


-- | Convert to a ByteString.
--
-- Nix exports are a binary format. The input to this function is
-- a bytestring intended to have been created from a call to
-- `nix-store --dump`, or equivalently, as returned by a nix
-- binary cache. The logic of this function adds a few things:
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
--   "\x05\x00\x00\x00\x00\x00\x00\x00hello\x00\x00\x00"
--
-- Note that there are three zeros following the "hello" text, in
-- order to pad it to eight bytes.
narExportToBytestring :: NarExport -> ByteString
narExportToBytestring NarExport{..} = flip State.execState mempty $ do
  let add s = State.modify (<> s)
      -- Implementing the string-encoding logic described above.
      addString str = do
        let len = length str
        -- Add the length of a string, represented in 8 bytes.
        add $ toStrict $ toLazyByteString $ word64LE (fromIntegral len)
        -- Add the string.
        add str
        -- Add padding if necessary.
        add $ replicate (8 - (len `mod` 8)) 0
      addStorePath sp = addString $ B8.pack $ spToFull neStoreDir sp

  -- Magic 8-byte number nix expects at the beginning of an export.
  add _EXPORT_MAGIC_1
  add $ narToBytestring neNar
  -- Another magic 8-byte number that comes after the NAR.
  add _EXPORT_MAGIC_2
  -- Add the store path of the object itself, followed by its references.
  mapM addStorePath (neStorePath : neReferences)
  -- Add the deriver path, if it's present. Otherwise an empty string.
  maybe (addString "") addStorePath neDeriver
  -- Add 16 zeros: 8 to indicate no signature, and then another 8 to
  -- indicate the end of the export.
  add $ replicate 16 0
