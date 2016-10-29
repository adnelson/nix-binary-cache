-- | Haskell representation of nix store paths.
module Nix.StorePath where

import ClassyPrelude hiding (try)
import Text.Parsec
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Text.Regex.PCRE.Heavy (scan, re)

-- | The nix store directory.
newtype NixStoreDir = NixStoreDir FilePath
  deriving (Show, Eq, Generic, Hashable)

-- | The hash and name of an object in the nix store.
data StoreBasepath = StoreBasepath Text Text
  deriving (Show, Eq, Generic)

instance Hashable StoreBasepath

-- | Path to an object in the nix store.
data StorePath = StorePath NixStoreDir StoreBasepath
  deriving (Show, Eq, Generic)

instance Hashable StorePath

spStoreDir :: StorePath -> FilePath
spStoreDir (StorePath (NixStoreDir p) _) = p

spObjectHash :: StorePath -> Text
spObjectHash (StorePath _ (StoreBasepath hash _)) = hash

spObjectName :: StorePath -> Text
spObjectName (StorePath _ (StoreBasepath _ name)) = name

-- | Get the basename of a store path.
spBasename :: StorePath -> FilePath
spBasename (StorePath _ sbp) = sbpRender sbp

-- | Get the full path of a store path.
spFullpath :: FilePath -> StoreBasepath -> FilePath
spFullpath storeDir sbp = storeDir </> sbpRender sbp

parseStoreBasepath :: Text -> Either String StoreBasepath
parseStoreBasepath txt =
  case scan [re|^([\w\d]{32})-(.*)|] txt of
    [(_, [hash, name])] -> Right $ StoreBasepath hash name
    _ -> Left $ show txt <> " does not appear to be a store basepath"

-- | Parse a store path from text. Probably not super efficient but oh well.
parseStorePath :: Text -> Either String StorePath
parseStorePath txt = case unsnoc $ T.split (=='/') txt of
  Nothing -> Left $ show txt <> " does not appear to be a store path"
  Just (intercalate "/" -> unpack -> storeDir, pathInStore) -> do
    basepath <- parseStoreBasepath pathInStore
    return $ StorePath (NixStoreDir storeDir) basepath

ioParseStoreBasepath :: Text -> IO StoreBasepath
ioParseStoreBasepath txt = case parseStoreBasepath txt of
  Left err -> error err
  Right sp -> return sp

ioParseStorePath :: Text -> IO StorePath
ioParseStorePath txt = case parseStorePath txt of
  Left err -> error err
  Right sp -> return sp

sbpRender :: StoreBasepath -> FilePath
sbpRender (StoreBasepath hash name) = unpack $ hash <> "-" <> name
