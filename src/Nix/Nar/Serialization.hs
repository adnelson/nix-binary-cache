{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Serialization of Nars and NarExports
module Nix.Nar.Serialization where

import ClassyPrelude hiding (take, try, Builder)
import Data.Binary
import Data.Binary.Get (getInt64le, getByteString, skip, lookAhead, label)
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (Put, putByteString, putInt64le, execPut)
import Data.ByteString.Builder (toLazyByteString)
-- import Data.Attoparsec.ByteString (Parser, take, many', try, choice, string)
-- import Data.Attoparsec.Lazy (Result(Fail, Done), parse)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
-- import qualified Data.ByteString.Lazy as BL
import Servant (MimeUnrender(..), OctetStream, MimeRender(..))


import Nix.StorePath (NixStoreDir(..), StorePath(..))
import Nix.StorePath (parseFullStorePath, spToFull)
import Nix.Nar.Types

-- | Wrap the Int64 type to create custom Binary instance
newtype NarInt = NarInt Int deriving (Show, Eq, Ord, Num)

-- NarInts are written as a 8 bytes in little endian format
instance Binary NarInt where
  put (NarInt n) = putInt64le $ fromIntegral n
  get = NarInt . fromIntegral <$> getInt64le

-- | Wrap to create custom Binary instance
newtype NarString = NarString ByteString deriving (Show, Eq, Ord, IsString)
instance Binary NarString where
  put (NarString s) = put (NarInt $ length s) *> putByteString (padTo8 s)
    where padTo8 bs | length bs `mod` 8 == 0 = bs
          padTo8 bs = bs <> replicate (8 - (length bs `mod` 8)) 0

  get = do
    -- Get the length of the string
    NarInt len <- get
    -- Read that many bytes
    result <- NarString <$> getByteString len
    -- Read any bytes remaining (padded to a multiple of 8)
    when (len `mod` 8 /= 0) $ do
      skip (8 - (len `mod` 8))
    pure result

-- | Convenience function to resolve type ambiguity.
putNS :: NarString -> Put
putNS = put

getExactNS :: NarString -> Get ()
getExactNS expected = do
  s <- get
  when (s /= expected) $ do
    fail ("expected string " <> show expected <> " but got " <> show s)

getSomeNS :: Get ByteString
getSomeNS = get >>= \(NarString s) -> pure s

instance Binary NarElement where
  put element = inParens internal where
    inParens p = putNS "(" *> p *> putNS ")"
    internal = case element of
      NarSymLink target -> do
        mapM_ putNS ["type", "symlink", "target", NarString target]
      NarFile exec contents -> do
        mapM_ putNS ["type", "regular"]
        when (exec == Executable) $ do
          mapM_ putNS ["executable", ""]
        mapM_ putNS ["contents", NarString contents]
      NarDirectory elements -> do
        mapM_ putNS ["type", "directory"]
        forM_ (sortOn fst $ H.toList elements) $ \(name, element) -> do
          putNS "entry"
          inParens $ do
            mapM_ putNS ["name", NarString name, "node"]
            put element

  get = label "NarElement" $ inParens element where
    inParens p = label "openParens" (getExactNS "(") *> p
                 <* label "closeParens" (getExactNS ")")
    try' getter = lookAhead getter >> getter
    many_ p results = (p >>= \result -> many_ p (result:results))
                      <|> pure results
    getDir = H.fromList <$> many_ entry [] where
      entry = do
        try' (getExactNS "entry")
        inParens $ do
          NarString name <- getExactNS "name" *> get
          element <- getExactNS "node" *> get
          pure (name, element)
    element = do
      getExactNS "type"
      get >>= \case
        "directory" -> NarDirectory <$> getDir
        "symlink" -> NarSymLink <$> (getExactNS "target" *> getSomeNS)
        "regular" -> do
          isExecutable <- do
            (try' (mapM getExactNS ["executable", ""]) *> pure Executable)
              <|> pure NotExecutable
          NarFile isExecutable <$> (getExactNS "contents" *> getSomeNS)
        (t :: NarString) -> do
          fail ("unsupported element type: " <> show t)

instance Binary Nar where
  get = label "Nar" $ Nar <$> (getExactNS "nix-archive-1" *> get)
  put (Nar elem) = putNS "nix-archive-1" >> put elem

getThisByteString :: ByteString -> Get ()
getThisByteString expected = do
  s <- getByteString (length expected)
  when (s /= expected) $ do
    fail ("expected string " <> show expected <> " but got " <> show s)

putStorePath :: NixStoreDir -> StorePath -> Put
putStorePath sd sp = putNS $ NarString $ B8.pack $ spToFull sd sp


-- | Magic constant at the beginning of an export
magicExportStartConstant :: ByteString
magicExportStartConstant = B.pack (1 : replicate 7 0)

-- | Magic constant to indicate start of export metadata
magicExportMetadataConstant :: ByteString
magicExportMetadataConstant = "NIXE" <> B.pack (replicate 4 0)

getStorePath :: Get (NixStoreDir, StorePath)
getStorePath = do
  NarString s <- get
  case parseFullStorePath (decodeUtf8 s) of
    Left err -> fail err
    Right (sd, sp) -> pure (sd, sp)

instance Binary NarExport where
  put (NarExport {..}) = do
    -- Write the NAR surrounded by constants
    putByteString magicExportStartConstant
    put neNar
    putByteString magicExportMetadataConstant

    -- Write the store path
    put (NarString $ B8.pack $ spToFull neStoreDirectory neStorePath)

    -- Write the references
    put (NarInt $ length neReferences)
    forM neReferences $ \sp -> do
      put (NarString $ B8.pack $ spToFull neStoreDirectory sp)

    -- If there's a deriver, write it. Otherwise an empty string
    put $ case neDeriver of
      Nothing -> ""
      Just sp -> NarString $ B8.pack $ spToFull neStoreDirectory sp

    -- If no signature, put 0, else 1 and then the signature
    case neSignature of
      Nothing -> put (NarInt 0)
      Just sig -> put (NarInt 1) *> put (NarString sig)

    -- The end of the export is eight zeroes
    putByteString $ B.replicate 8 0

  get = do
    -- Read the NAR surrounded by constants
    getThisByteString magicExportStartConstant
    neNar <- get
    getThisByteString magicExportMetadataConstant

    -- Get the store path of the exported object
    (neStoreDirectory, neStorePath) <- getStorePath
    -- Get the references
    neReferences <- do
      NarInt numReferences <- get
      forM [0 .. (numReferences - 1)] $ \_ -> do
        snd <$> getStorePath
    -- Get the deriver (optional)
    neDeriver <- getSomeNS >>= \case
      "" -> pure Nothing
      raw -> case parseFullStorePath (decodeUtf8 raw) of
        Left err -> fail err
        Right (_, path) -> pure $ Just path
    -- Get the signature (optional)
    neSignature <- get >>= \case
      (0 :: NarInt) -> pure Nothing
      1 -> Just <$> getSomeNS
      n -> fail ("Expected either 0 or 1 before the signature, got " <> show n)

    -- Consume the final 8 bytes
    getByteString 8

    pure NarExport {..}

instance MimeRender OctetStream Nar where
  mimeRender _ = toLazyByteString . execPut . put

instance MimeUnrender OctetStream Nar where
  mimeUnrender _ bs = case runGetOrFail get bs of
    Right (_, _, nar) -> pure nar
    Left (_, _, err) -> Left err

instance MimeRender OctetStream NarExport where
  mimeRender _ = toLazyByteString . execPut . put

instance MimeUnrender OctetStream NarExport where
  mimeUnrender _ bs = case runGetOrFail get bs of
    Right (_, _, export) -> pure export
    Left (_, _, err) -> Left err
