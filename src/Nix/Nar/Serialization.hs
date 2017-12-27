{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Serialization of Nars and NarExports
module Nix.Nar.Serialization where

import ClassyPrelude hiding (take, try, Builder)
import Data.Binary
import Data.Binary.Get (getInt64le, getByteString, skip, lookAhead, label)
import Data.Binary.Put (Put, putByteString, putInt64le, execPut)
import Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import Data.Attoparsec.ByteString (Parser, take, many', try, choice, string)
import Data.Attoparsec.Lazy (Result(Fail, Done), parse)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Servant (MimeUnrender(..), OctetStream, MimeRender(..))


import Nix.StorePath (NixStoreDir(..), StorePath(..))
import Nix.StorePath (parseFullStorePath, spToFull)
import Nix.Nar.Types

parseNumber :: Parser Int
parseNumber = do
  let step (r, m) d = (r + m * fromIntegral d, m * 256)
      calc bs = fst $ B.foldl' step (0, 1) bs
  calc <$> take 8

parseSomeString :: Parser B.ByteString
parseSomeString = do
  stringlength <- parseNumber
  result <- take stringlength
  -- string length is padded to a multiple of 8, so consume remaining input
  when (stringlength `mod` 8 /= 0) $ do
    void $ take (8 - stringlength `mod` 8)
  pure result

parseString :: B.ByteString -> Parser ()
parseString bs = parseSomeString >>= \case
  bs' | bs == bs' -> pure ()
  bs' -> fail ("expected string " <> show bs <> " but got " <> show bs')

parseParens :: Parser a -> Parser a
parseParens p = parseString "(" *> p <* parseString ")"

parseDirectory :: Parser (HashMap B.ByteString NarElement)
parseDirectory = H.fromList <$> many' entry where
  entry = try $ do
    parseString "entry"
    parseParens $ do
      name <- parseString "name" *> parseSomeString
      element <- parseString "node" *> parseElement
      pure (name, element)

parseElement :: Parser NarElement
parseElement = parseParens $ do
  parseString "type"
  parseSomeString >>= \case
    "directory" -> NarDirectory <$> parseDirectory
    "symlink" -> NarSymLink <$> (parseString "target" *> parseSomeString)
    "regular" -> do
      -- Figure out if this is executable or not
      isExecutable <- choice [
        parseString "executable" *> parseString "" *> pure Executable,
        pure NotExecutable
        ]
      NarFile isExecutable <$> (parseString "contents" *> parseSomeString)
    t -> do
      fail ("unsupported element type: " <> show t)

parseNar :: Parser Nar
parseNar = Nar <$> (parseString "nix-archive-1" *> parseElement)

parseStorePathAndNixStore :: Parser (NixStoreDir, StorePath)
parseStorePathAndNixStore = do
  raw <- parseSomeString
  case parseFullStorePath (decodeUtf8 raw) of
    Left err -> fail err
    Right result -> pure result

parseNarExport :: Parser NarExport
parseNarExport = do
  -- magic constant at the beginning of an export
  string $ B.pack (1 : replicate 7 0)
  -- after that comes the NAR itself
  neNar <- parseNar
  -- magic constant for the export metadata
  string $ "NIXE" <> B.replicate 4 0
  -- Get the store path of the exported object
  (neStoreDirectory, neStorePath) <- parseStorePathAndNixStore
  -- Get the references
  neReferences <- do
    numReferences <- parseNumber
    forM [0 .. (numReferences - 1)] $ \_ -> do
      snd <$> parseStorePathAndNixStore
  -- Get the deriver (optional)
  neDeriver <- parseSomeString >>= \case
    "" -> pure Nothing
    raw -> case parseFullStorePath (decodeUtf8 raw) of
      Left err -> fail err
      Right (_, path) -> pure $ Just path
  -- Get the signature (optional)
  neSignature <- parseNumber >>= \case
    0 -> pure Nothing
    1 -> Just <$> parseSomeString
    n -> fail ("Expected either 0 or 1 before the signature, got " <> show n)
  pure NarExport {..}

parseConsumeInput :: Parser t -> BL.ByteString -> Either String t
parseConsumeInput parser bs = case parse parser bs of
  Done "" result -> pure result
  Done _ _ -> Left "There is remaining input"
  Fail _ _ err -> Left err

instance MimeUnrender OctetStream Nar where
  mimeUnrender _ = parseConsumeInput parseNar

instance MimeUnrender OctetStream NarExport where
  mimeUnrender _ = parseConsumeInput parseNarExport


-- | Convert an integer to a bytestring, 8 little-endian word8s
intToChunks :: Int -> ByteString -- [Word8]
intToChunks n = pack $ reverse $ go (1 :: Integer) n [] where
  go 9 _ octets = octets
  go k n' octets = go (k + 1) (n' `div` 256)
                      (fromIntegral (n' `mod` 256) : octets)

-- | Zero-pad a bytestring so that its length is a multiple of 8.
padTo8 :: ByteString -> ByteString
padTo8 bs | length bs `mod` 8 == 0 = bs
padTo8 bs = bs <> replicate (8 - (length bs `mod` 8)) 0

narBuildString :: ByteString -> Builder
narBuildString bs = do
  byteString (intToChunks $ length bs) <> byteString (padTo8 bs)

elementToBuilder :: NarElement -> Builder
elementToBuilder element = do
  let
    internal = case element of
      NarSymLink target -> concat [narBuildString "type",
                                   narBuildString "symlink",
                                   narBuildString "target",
                                   narBuildString target]
      NarFile NotExecutable contents -> concat [narBuildString "type",
                                                narBuildString "regular",
                                                narBuildString "contents",
                                                narBuildString contents]
      NarFile Executable contents -> concat [narBuildString "type",
                                             narBuildString "regular",
                                             narBuildString "executable",
                                             narBuildString "",
                                             narBuildString "contents",
                                             narBuildString contents]
      NarDirectory elements -> concat $ [
        narBuildString "type",
        narBuildString "directory",
        concat $ do
          flip map (sortOn fst $ H.toList elements) $ \(name, element) -> do
            concat [
              narBuildString "entry",
              narBuildString "(",
              narBuildString "name",
              narBuildString name,
              narBuildString "node"
              ] <> elementToBuilder element <> narBuildString ")"
       ]
  narBuildString "(" <> internal <> narBuildString ")"

-- | Wrap the Int64 type to create custom Binary instance
newtype NarInt = NarInt Int deriving (Show, Eq, Ord, Num)

nlength :: MonoFoldable m => m -> NarInt
nlength = NarInt . length

-- NarInts are written as a 8 bytes in little endian format
instance Binary NarInt where
  put (NarInt n) = putInt64le $ fromIntegral n
  get = NarInt . fromIntegral <$> getInt64le

-- | Wrap to create custom Binary instance
newtype NarString = NarString ByteString deriving (Show, Eq, Ord, IsString)
instance Binary NarString where
  put (NarString s) = put (nlength s) *> putByteString (padTo8 s)
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
