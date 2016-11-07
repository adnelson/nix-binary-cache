-- | Provides basic utilites to the cache libraries.
module Nix.Cache.Common (
  module ClassyPrelude,
  listDirectory, splitWS
  ) where

import ClassyPrelude
import qualified Data.Text as T
import System.Directory (getDirectoryContents)

-- | Split a text on whitespace. Derp.
splitWS :: Text -> [Text]
splitWS = filter (/= "") . T.split (flip elem [' ', '\t', '\n', '\r'])

-- | This function is not in all versions of the directory package, so
-- we copy/paste the definition here.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
