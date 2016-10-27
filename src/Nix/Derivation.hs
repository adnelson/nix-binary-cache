-- | Nix derivations.
module Nix.Derivation where

import Prelude (read)
import ClassyPrelude
import Text.Parsec
import Text.Parsec.Token
import qualified Data.HashMap.Strict as H

type Parser a = Parsec [Char] () a

-- | Objects in the nix store.
newtype StorePath = StorePath Text
  deriving (Show, Eq, Generic)

toFullPath :: FilePath -- ^ Path to the nix store.
           -> StorePath -- ^ Store path.
           -> FilePath -- ^ Full path to the store object.
toFullPath pathToStore (StorePath p) = pathToStore </> unpack p

data Derivation = Derivation {
  derivOutputs :: HashMap Text StorePath,
  -- ^ Outputs the derivation is expected to produce and what they're called.
  derivInputDerivations :: HashMap StorePath [Text],
  -- ^ Derivations this derivation needs to have as inputs, and
  -- outputs of those derivations.
  derivInputPaths :: [StorePath],
  -- ^ Non-derivation inputs the derivation needs in order to build.
  derivBuilder :: StorePath,
  -- ^ Path to the executable to build the derivation.
  derivArgs :: [Text],
  -- ^ Arguments to the builder.
  derivEnv :: HashMap Text Text,
  -- ^ Environment to run the builder in.
  derivSystem :: Text
  -- ^ System the derivation is to be built on.
  } deriving (Show, Eq, Generic)

-- | Parses a string constant.
parseText :: Parser Text
parseText = char '"' >> loop [] where
  loop acc = do
    let continue c = loop (c:acc)
    anyChar >>= \case
      '"' -> return $ pack $ reverse acc
      '\\' -> anyChar >>= \case
        'n' -> continue '\n'
        'r' -> continue '\r'
        't' -> continue '\t'
        'b' -> continue '\b'
        c -> continue c
      c -> continue c

surround :: Char -> Char -> Parser a -> Parser a
surround start stop p = char start *> p <* char stop

parseDerivation :: Parser (HashMap Text Text)
parseDerivation = do
  string "Derive"
  surround '(' ')' $ do
    outs <- surround '[' ']' $ flip sepBy1 (char ',') $ do
      surround '(' ')' $ do
        outName <- parseText
        char ','
        outPath <- parseText
        string ",\"\",\"\""
        return (outName, outPath)
    return $ H.fromList outs
--  read <$> many1 digit
