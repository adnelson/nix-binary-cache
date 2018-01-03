-- | Miscellaneous utility and/or tester functions
module Nix.Cache.Client.Misc where

import Nix.Cache.Common
import Nix.StorePath (NixStoreDir(..), StorePath)
import Nix.StorePath (getNixStoreDir, parseStorePath)


getNixStorePaths :: Int -> IO [StorePath]
getNixStorePaths count = do
  NixStoreDir d <- getNixStoreDir
  list <- listDirectory d
  pure $ take count $ catMaybes $ flip map list $ \txt -> do
    case parseStorePath (pack txt) of
      Left _ -> Nothing
      Right sp -> Just sp
