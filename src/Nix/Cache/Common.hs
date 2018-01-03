-- | Provides basic utilites to the cache libraries.
module Nix.Cache.Common (
  module ClassyPrelude,
  listDirectory, splitWS, errorIs404
  ) where

import ClassyPrelude
import qualified Data.Text as T
import System.Directory (getDirectoryContents)
import Network.HTTP.Types.Status (Status(..))
import Servant.Client (ServantError(..))

-- | Split a text on whitespace. Derp.
splitWS :: Text -> [Text]
splitWS = filter (/= "") . T.split (flip elem [' ', '\t', '\n', '\r'])

-- | This function is not in all versions of the directory package, so
-- we copy/paste the definition here.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

-- | Return true if a servant error is a 404.
errorIs404 :: ServantError -> Bool
#if MIN_VERSION_servant_client(0,11,0)
errorIs404 (FailureResponse _ (Status 404 _) _ _) = True
#else
errorIs404 (FailureResponse (Status 404 _) _ _) = True
#endif
errorIs404 _ = False
