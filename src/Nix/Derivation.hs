-- | Nix derivations.
module Nix.Derivation where

import ClassyPrelude
import Text.Parsec
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

-- | Path to an object in the nix store.
newtype StorePath = StorePath Text
  deriving (Show, Eq, Generic, Hashable)

-- | A representation of a hash, which expresses the type of
-- hash. This is encoded as a string in the form "<type>:<hash>",
-- where <type> is sha256, sha1, or md5. The <hash> part might be
-- encoded in hex or in base32. If a recursive hash, 'r:' is
-- prepended to the string representation.
data FileHash
  = Sha256Hash Text -- ^ Hash computed with sha256.
  | Sha1Hash Text -- ^ Hash computed with sha256.
  | Md5Hash Text -- ^ Hash computed with sha256.
  | RecursiveHash FileHash -- ^ Hash should be computed over a directory.
  deriving (Show, Eq, Generic)

-- | Translate text into a FileHash object.
fileHashFromText :: Text -> Either String FileHash
fileHashFromText txt = case T.split (==':') txt of
  [hashtype, hash] -> getFileHashConstructor hashtype <*> pure hash
  _ -> Left $ "Not a hash string: " <> show txt

-- | Given the identifier of a hash type, convert it into a FileHash
-- constructor (or fail).
getFileHashConstructor :: Text -> Either String (Text -> FileHash)
getFileHashConstructor txt = case unpack txt of
  'r':':':htype -> (.) RecursiveHash <$> go htype
  htype -> go htype
  where
    go "sha256" = return Sha256Hash
    go "sha1" = return Sha1Hash
    go "md5" = return Md5Hash
    go s = Left $ "Unknown hash type: " <> show s

toFullPath :: FilePath -- ^ Path to the nix store.
           -> StorePath -- ^ Store path.
           -> FilePath -- ^ Full path to the store object.
toFullPath pathToStore (StorePath p) = pathToStore </> unpack p

-- | The main derivation type. This represents all of the information
-- that is needed to construct a particular store object; the store
-- object(s) that will be built are listed in the `derivOutputs` field.
data Derivation = Derivation {
  derivOutputs :: HashMap Text (StorePath, Maybe FileHash),
  -- ^ Outputs the derivation is expected to produce and what they're
  -- called. Those outputs might have known hashes (fixed-output
  -- derivations); if so include those.
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

-- | Parsec parser type.
type Parser a = Parsec [Char] () a

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
    -- Or if the output has a known hash, then the hash type and hash:
    -- [("out","/nix/store/xyz-foo","sha256","abc123")]
    outs <- brackets $ sepCommas1 $ do
      parens $ do
        outName <- text
        char ','
        outPath <- StorePath <$> text
        char ','
        text >>= \case
          "" -> do
            -- If the next text is empty, it means this isn't a
            -- fixed-output hash. Then the next string should also be
            -- empty, and that's the end.
            string ",\"\""
            return (outName, (outPath, Nothing))
          hashtype -> case getFileHashConstructor hashtype of
            -- If it's not empty, then it should correspond to a valid
            -- hash type, and there should be some non-empty hash
            -- string coming next.
            Left err -> fail err
            Right constructor -> do
              char ','
              hash <- text
              return (outName, (outPath, Just $ constructor hash))
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

-- | Parse a derivation string.
parseDerivString :: String -> Either String Derivation
parseDerivString s = case parse derivationParser "derivation" s of
  Left err -> Left $ show err
  Right deriv -> Right deriv

-- | Parse a derivation file. Assumes the file exists.
parseDerivFile :: FilePath -> IO (Either String Derivation)
parseDerivFile p = parseDerivString <$> readFile p
