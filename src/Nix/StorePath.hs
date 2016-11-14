-- | Haskell representation of nix store paths.
module Nix.StorePath where

import ClassyPrelude hiding (try)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Text.Regex.PCRE.Heavy (scan, re)
import System.Process (readCreateProcess, shell)
import System.Environment (getEnv)
import Servant (MimeUnrender(..), OctetStream)

-- | The nix store directory.
newtype NixStoreDir = NixStoreDir FilePath
  deriving (Show, Eq, Generic, Hashable)

-- | The hash and name of an object in the nix store.
data StorePath = StorePath Text Text
  deriving (Show, Eq, Generic)

instance Hashable StorePath

-- | Path to an object in the nix store.
type FullStorePath = (NixStoreDir, StorePath)

-- | Read the NIX_STORE variable to get the path to the nix store.
getNixStoreDir :: IO NixStoreDir
getNixStoreDir = NixStoreDir <$> getEnv "NIX_STORE"

-- | Parse a nix store path from text. The input text should be a
-- basepath, not a full path (i.e., it should not be
-- '/nix/store/xyz-foo', but instead should be 'xyz-foo').
parseStorePath :: Text -> Either String StorePath
parseStorePath txt =
  case scan [re|^([\w\d]{32})-(.*)|] txt of
    [(_, [hash, name])] -> Right $ StorePath hash name
    _ -> Left $ show txt <> " does not appear to be a store basepath"

-- | Parse a store path from text. Probably not super efficient but oh well.
parseFullStorePath :: Text -> Either String (NixStoreDir, StorePath)
parseFullStorePath txt = case unsnoc $ T.split (=='/') txt of
  Nothing -> Left $ show txt <> " does not appear to be a store path"
  Just (intercalate "/" -> unpack -> storeDir, pathInStore) -> do
    basepath <- parseStorePath pathInStore
    return (NixStoreDir storeDir, basepath)

-- | Parse a StorePath in the IO monad.
ioParseStorePath :: Text -> IO StorePath
ioParseStorePath txt = case parseStorePath txt of
  Left err -> error err
  Right sp -> return sp

-- | Given a nix store dir and a store path, produce a full file path.
spToFull :: NixStoreDir -> StorePath -> FilePath
spToFull (NixStoreDir storeDir) p = storeDir </> spToPath p

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

instance MimeUnrender OctetStream StorePath where
  mimeUnrender _ = map snd . parseFullStorePath . T.decodeUtf8 . toStrict
