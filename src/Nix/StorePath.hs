-- | Haskell representation of nix store paths.
module Nix.StorePath where

import ClassyPrelude hiding (try)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Regex.PCRE.Heavy (scan, re)
import System.Process (readCreateProcess, shell)
import System.Environment (getEnv, lookupEnv)
import Servant (MimeUnrender(..), OctetStream)
import Servant.HTML.Lucid (HTML)
import System.FilePath (takeDirectory)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import qualified System.Process.ByteString as PB
import qualified System.Process.Text as PT
import qualified Data.ByteString.Char8 as B8

-- | The nix store directory.
newtype NixStoreDir = NixStoreDir FilePath
  deriving (Show, Eq, Generic, Hashable, IsString)

-- | The hash and name of an object in the nix store.
data StorePath = StorePath Text Text
  deriving (Show, Eq, Ord, Generic)

instance Hashable StorePath

-- | A dependency tree, represented as a mapping from a store path to
-- its set of (immediate, not transitive) dependent paths.
type PathTree = HashMap StorePath [StorePath]

-- | A set of store paths.
type PathSet = HashSet StorePath

-- | Path to an object in the nix store.
type FullStorePath = (NixStoreDir, StorePath)

-- | Read the NIX_STORE variable to get the path to the nix store.
getNixStoreDir :: IO NixStoreDir
getNixStoreDir = NixStoreDir <$> getEnv "NIX_STORE"

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

-- | Call `nix-store` with the given arguments, return a ByteString.
nixStoreBS :: NixBinDir -> [String] -> IO ByteString
nixStoreBS (NixBinDir nixBin) args = do
  PB.readProcessWithExitCode (nixBin </> "nix-store") args "" >>= \case
    (ExitSuccess, stdout, _) -> pure stdout
    (ExitFailure code, _, stderr) -> error $ unlines $ [
        cmd <> " failed with " <> show code, "STDERR:", B8.unpack stderr]
      where cmd = "nix-store " <> intercalate " " args

-- | Call `nix-store` with the given arguments, return Text.
nixStoreText :: NixBinDir -> [String] -> IO Text
nixStoreText (NixBinDir nixBin) args = do
  PT.readProcessWithExitCode (nixBin </> "nix-store") args "" >>= \case
    (ExitSuccess, stdout, _) -> pure stdout
    (ExitFailure code, _, stderr) -> error $ unlines $ [
        cmd <> " failed with " <> show code, "STDERR:", unpack stderr]
      where cmd = "nix-store " <> intercalate " " args

-- | Parse a nix store path from text. The input text should be a
-- basepath, not a full path (i.e., it should not be
-- '/nix/store/xyz-foo', but instead should be 'xyz-foo').
parseStorePath :: Text -> Either String StorePath
parseStorePath txt =
  case scan [re|^([\w\d]{32})-(.*)|] txt of
    [(_, [hash, name])] -> Right $ StorePath hash name
    _ -> Left $ show txt <> " does not appear to be a store basepath"

-- | Parse a store path from text. Probably not super efficient but oh well.
parseFullStorePath :: Text -> Either String FullStorePath
parseFullStorePath txt = case unsnoc $ T.split (=='/') txt of
  Nothing -> Left $ show txt <> " does not appear to be a store path"
  Just (intercalate "/" -> unpack -> storeDir, pathInStore) -> do
    basepath <- parseStorePath pathInStore
    return (NixStoreDir storeDir, basepath)

-- | Parse a StorePath in the IO monad.
ioParseStorePath :: MonadIO io => Text -> io StorePath
ioParseStorePath txt = liftIO $ case parseStorePath txt of
  Left err -> error err
  Right sp -> return sp

-- | Parse a full store path in the IO monad.
ioParseFullStorePath :: MonadIO io => Text -> io FullStorePath
ioParseFullStorePath txt = liftIO $ case parseFullStorePath txt of
  Left err -> error err
  Right result -> return result

-- | Given a nix store dir and a store path, produce a full file path.
spToFull :: NixStoreDir -> StorePath -> FilePath
spToFull (NixStoreDir storeDir) p = storeDir </> spToPath p

-- | Same as above, but uses a FullStorePath, when that's more concise.
spToFull' :: FullStorePath -> FilePath
spToFull' = uncurry spToFull

-- | Convert a StorePath to a FilePath.
spToPath :: StorePath -> FilePath
spToPath (StorePath hash name) = unpack $ hash <> "-" <> name

-- | Find a nix store path by its store prefix. If multiple paths
-- satisfy the prefix, the first one will be taken.
findSpByPrefix :: Text -> IO StorePath
findSpByPrefix prefix = do
  NixStoreDir dir <- getNixStoreDir
  let cmd = "ls " <> dir <> " | grep '^" <> unpack prefix <> "'"
  result <- readCreateProcess (shell cmd) ""
  case parseFullStorePath $ pack result of
    Left err -> error err
    Right (_, sp) -> return sp

-- | Find a nix store path by suffix. If multiple paths satisfy the
-- suffix, the first one will be taken.
findSpBySuffix :: Text -> IO StorePath
findSpBySuffix prefix = do
  NixStoreDir dir <- getNixStoreDir
  let cmd = "ls " <> dir <> " | grep '" <> unpack prefix <> "$'"
  result <- readCreateProcess (shell cmd) ""
  case parseFullStorePath $ pack result of
    Left err -> error err
    Right (_, sp) -> return sp

-- | Find a nix store path by some text that appears in the path. If
-- multiple paths satisfy the search, the first one will be taken.
findSp :: Text -> IO StorePath
findSp text = do
  NixStoreDir dir <- getNixStoreDir
  let cmd = "ls " <> dir <> " | grep '" <> unpack text <> "'"
  result <- readCreateProcess (shell cmd) ""
  case parseFullStorePath $ pack result of
    Left err -> error err
    Right (_, sp) -> return sp

-- | Return an abbreviated version of a store path, e.g. for
-- debugging. Uses only the first 6 characters of the prefix.
abbrevSP :: StorePath -> Text
abbrevSP (StorePath hash name) = T.take 6 hash <> "-" <> name

instance MimeUnrender OctetStream StorePath where
  mimeUnrender _ = map snd . parseFullStorePath . T.decodeUtf8 . toStrict

instance MimeUnrender HTML StorePath where
  mimeUnrender _ = map snd . parseFullStorePath . T.decodeUtf8 . toStrict
