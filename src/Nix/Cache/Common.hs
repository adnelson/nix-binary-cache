-- | Provides basic utilites to the cache libraries.
module Nix.Cache.Common (
  module ClassyPrelude,
  splitWS
  ) where

import ClassyPrelude
import qualified Data.Text as T

-- | Split a text on whitespace. Derp.
splitWS :: Text -> [Text]
splitWS = filter (/= "") . T.split (flip elem [' ', '\t', '\n', '\r'])
