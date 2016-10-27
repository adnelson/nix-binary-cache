-- | Nix derivations.
module Nix.Derivation where

import ClassyPrelude
import Text.Parsec
import qualified Data.HashMap.Strict as H

type Parser a = Parsec [Char] () a

-- | Path to an object in the nix store.
newtype StorePath = StorePath Text
  deriving (Show, Eq, Generic)

instance Hashable StorePath

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
  derivSystem :: Text,
  -- ^ System the derivation is to be built on.
  derivBuilder :: StorePath,
  -- ^ Path to the executable to build the derivation.
  derivArgs :: [Text],
  -- ^ Arguments to the builder.
  derivEnv :: HashMap Text Text
  -- ^ Environment to run the builder in.
  } deriving (Show, Eq, Generic)

-- | Parses a string constant.
text :: Parser Text
text = char '"' >> loop [] where
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

-- | Execute a parser surrounded by two characters.
surround :: Char -> Char -> Parser a -> Parser a
surround start stop p = char start *> p <* char stop

-- | Parse a derivation in the Parser monad.
derivationParser :: Parser Derivation
derivationParser = do
  let parens = surround '(' ')'
      brackets = surround '[' ']'
      sepCommas = flip sepBy (char ',')
      sepCommas1 = flip sepBy1 (char ',')
      textList = brackets $ sepCommas text
  -- All derivations start with this string.
  string "Derive"
  parens $ do
    -- Grab the output list. This is a comma-separated list of
    -- 4-tuples, like so:
    -- [("out","/nix/store/sldkfjslkdfj-foo","","")]
    outs <- brackets $ sepCommas1 $ do
      parens $ do
        outName <- text
        char ','
        outPath <- StorePath <$> text
        string ",\"\",\"\""
        return (outName, outPath)
    char ','
    -- Grab the input derivation list. A comma-separated list of
    -- 2-tuples like so:
    -- [("/nix/store/abc-bar",["out"]), ("/nix/store/xyz-bux",["out","dev"])]
    inDerivs <- brackets $ sepCommas $ do
      parens $ do
        inDName <- StorePath <$> text
        char ','
        inDOutputs <- textList
        return (inDName, inDOutputs)
    -- Grab the input file list (not derivations). Just a list of
    -- strings.
    inFiles <- char ',' >> map StorePath <$> textList
    -- Grab the system info string.
    system <- char ',' >> text
    -- Grab the builder executable path.
    builder <- char ',' >> StorePath <$> text
    -- Grab the builder arguments.
    builderArgs <- char ',' >> textList
    -- Grab the build environment, a list of 2-tuples.
    char ','
    buildEnv <- brackets $ sepCommas $ parens $ do
      key <- text
      value <- char ',' *> text
      return (key, value)
    return $ Derivation {
      derivOutputs = H.fromList outs,
      derivInputDerivations = H.fromList inDerivs,
      derivInputPaths = inFiles,
      derivSystem = system,
      derivBuilder = builder,
      derivArgs = builderArgs,
      derivEnv = H.fromList buildEnv
      }
