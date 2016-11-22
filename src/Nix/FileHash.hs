-- | Text representation of file hashes.
module Nix.FileHash where

import ClassyPrelude
import qualified Data.Text as T

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

-- | Translate a file hash to text.
fileHashToText :: FileHash -> Text
fileHashToText = \case
  Sha256Hash t -> "sha256:" <> t
  Sha1Hash t -> "sha1:" <> t
  Md5Hash t -> "md5:" <> t
  RecursiveHash h -> "r:" <> fileHashToText h

-- | Translate text into a FileHash object.
fileHashFromText :: Text -> Either String FileHash
fileHashFromText txt = case T.split (==':') txt of
  "r":rest -> RecursiveHash <$> fileHashFromText (intercalate ":" rest)
  [hashtype, hash] -> getFileHashConstructor hashtype <*> pure hash
  _ -> Left $ "Not a hash string: " <> show txt

-- | Given the identifier of a hash type, convert it into a FileHash
-- constructor (or fail).
getFileHashConstructor :: Text -> Either String (Text -> FileHash)
getFileHashConstructor txt = case unpack txt of
  'r':':':htype -> (RecursiveHash .) <$> go htype
  htype -> go htype
  where
    go "sha256" = return Sha256Hash
    go "sha1" = return Sha1Hash
    go "md5" = return Md5Hash
    go s = Left $ "Unknown hash type: " <> show s
