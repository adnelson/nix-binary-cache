-- | Defining a `KVMap` data structure, which is a string -> string
-- mapping that is text encoded in a simple format.
module Data.KVMap where

import ClassyPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Attoparsec.ByteString.Lazy (Parser)
import Data.Attoparsec.ByteString.Char8 (char, notChar, space, endOfLine,
                                         many1)

-- | Some nix cache information comes in a line-separated "Key: Value"
-- format. Here we represent that as a map.
newtype KVMap = KVMap (HashMap Text Text)
  deriving (Show, Eq, Generic)

-- | Class for things which can be represented in KVMaps.
class FromKVMap t where
  fromKVMap :: KVMap -> Either String t

-- | KVMaps can be parsed from text.
parseKVMap :: Parser KVMap
parseKVMap = do
  many $ endOfLine <|> (space >> return ())
  keysVals <- many $ do
    key <- many1 $ notChar ':'
    char ':' >> many space
    val <- many1 $ notChar '\n'
    many $ endOfLine <|> (space >> return ())
    return (T.pack key, T.pack val)
  return $ KVMap $ H.fromList keysVals
