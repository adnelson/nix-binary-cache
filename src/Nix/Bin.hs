-- | Execute a nix command.
module Nix.Bin where

import ClassyPrelude
import System.Environment (setEnv, lookupEnv)
import System.Exit (ExitCode(..))
import qualified System.Process.ByteString as PB
import qualified System.Process.ByteString.Lazy as PBL
import qualified System.Process.Text as PT
import System.FilePath (takeDirectory)
import System.Directory (doesFileExist)
import System.Process (readCreateProcess, shell)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

-- | Path to the directory containing nix binaries.
newtype NixBinDir = NixBinDir {unpackNixBinDir::FilePath}
  deriving (Show, Eq, IsString)

-- | Get the nix binary directory path, e.g. where `nix-store` lives.
getNixBinDir :: IO NixBinDir
getNixBinDir = lookupEnv "NIX_BIN_DIR" >>= \case
  Just dir -> doesFileExist (dir </> "nix-store") >>= \case
    True -> pure $ NixBinDir dir
    False -> findit
  Nothing -> findit
  where
    cmd = shell "which nix-store"
    findit = do
      dir <- takeDirectory <$> readCreateProcess cmd ""
      setEnv "NIX_BIN_DIR" dir
      pure $ NixBinDir dir

-- | Class for things which can be returned from nix commands.
class NixCmdReturn t where
  nixCmd :: NixBinDir -> String -> [String] -> BL8.ByteString -> IO t

-- | Run a nix command, using the `getNixBinDir` function
nixCmd' :: NixCmdReturn t => String -> [String] -> BL8.ByteString -> IO t
nixCmd' cmd args input = getNixBinDir >>= \d -> nixCmd d cmd args input

instance NixCmdReturn ByteString where
  nixCmd (NixBinDir nixBin) cmd args input = do
    let executable = (nixBin </> ("nix-" <> cmd))
    PB.readProcessWithExitCode executable args (toStrict input) >>= \case
      (ExitSuccess, stdout, _) -> pure stdout
      (ExitFailure code, _, stderr) -> error $ unlines $ [
          cmd' <> " failed with status " <> show code
          , "STDERR:", B8.unpack stderr]
        where cmd' = "nix-" <> cmd <> " " <> intercalate " " args

instance NixCmdReturn BL8.ByteString where
  nixCmd (NixBinDir nixBin) cmd args input = do
    let executable = (nixBin </> ("nix-" <> cmd))
    PBL.readProcessWithExitCode executable args input >>= \case
      (ExitSuccess, stdout, _) -> pure stdout
      (ExitFailure code, _, stderr) -> error $ unlines $ [
          cmd' <> " failed with status " <> show code
          , "STDERR:", BL8.unpack stderr]
        where cmd' = "nix-" <> cmd <> " " <> intercalate " " args

instance NixCmdReturn Text where
  nixCmd (NixBinDir nixBin) cmd args input = do
    let executable = (nixBin </> ("nix-" <> cmd))
    PB.readProcessWithExitCode executable args (toStrict input) >>= \case
      (ExitSuccess, stdout, _) -> pure (decodeUtf8 stdout)
      (ExitFailure code, _, stderr) -> error $ unlines $ [
          cmd' <> " failed with status " <> show code
          , "STDERR:", B8.unpack stderr]
        where cmd' = "nix-" <> cmd <> " " <> intercalate " " args

instance NixCmdReturn () where
  nixCmd nixBin cmd args input =
    (nixCmd nixBin cmd args input :: IO Text) >> pure ()
