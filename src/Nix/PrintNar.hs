module Main where

import ClassyPrelude
import Nix.Nar
-- import System.IO (stdin)
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

-- takeHeadChunk :: BL.ByteString -> Maybe B.ByteString
-- takeHeadChunk lbs =
--   case lbs of
--     (BL.Chunk bs _) -> Just bs
--     _ -> Nothing

-- -- | The simplest interface to run a 'Get' decoder. If the decoder runs into
-- -- an error, calls 'fail', or runs out of input, it will call 'error'.
-- runGet :: Get a -> BL.ByteString -> a
-- runGet g lbs0 = feedAll (runGetIncremental g) lbs0
--   where
--   feedAll (Done _ _ x) _ = x
--   feedAll (Partial k) lbs = feedAll (k (takeHeadChunk lbs)) (dropHeadChunk lbs)
--   feedAll (Fail _ pos msg) _ =
--     error ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)

files :: [String]
files = map (\n -> ("/home/anelson/" <> n)) [
  "fartdir.dump",
  "fart.dump",
  "haslink.dump",
  "kev.dump",
  "kev.haskell.dump",
  "xiz2.dump",
  "xiz3.dump",
  "xiz.dump"
  ]

go :: [Text] -> FilePath -> IO ()
go cmd path = do
  print path
  narBytes <- BL.readFile path
  case cmd of
    "raw":_ -> print narBytes
    "dump":_ -> do
      case runGetOrFail (get :: Get Nar) narBytes of
        Right (_, _, nar) -> do
          let serialized = runPut $ put nar
          case serialized == narBytes of
            True -> putStrLn $ tshow path <> " de/serialized correctly"
            False -> error $ path <> " did not de/serialize correctly"
        Left (_, c, e) -> do
          putStrLn $ "In file " <> tshow path <> ":"
          putStrLn $ "Character " <> tshow c <> ": " <> pack e
    _ -> do
      case runGetOrFail (get :: Get NarExport) narBytes of
        Right (_, _, export) -> do
          let serialized = runPut $ put export
          case serialized == narBytes of
            True -> putStrLn $ tshow path <> " de/serialized correctly"
            False -> do
              BL.writeFile "/home/anelson/error.export" serialized
              error $ path <> " did not de/serialize correctly"
        Left (_, c, e) -> do
          putStrLn $ "In file " <> tshow path <> ":"
          putStrLn $ "Character " <> tshow c <> ": " <> pack e


main :: IO ()
main = do
  forM_ files $ \name -> do
    (go ["dump"] name) `catch` \(e::SomeException) -> putStrLn (tshow name <> " failed: " <> tshow e)
