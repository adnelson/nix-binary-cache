-- | Execute a nix command.
module Nix.Bin where

import ClassyPrelude
import System.Environment (setEnv, lookupEnv)
import System.Exit (ExitCode(..))
import qualified System.Process.ByteString as PB
import qualified System.Process.Text as PT
import System.FilePath (takeDirectory)
import System.Directory (doesFileExist)
import System.Process (readCreateProcess, shell)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Nix.StorePath (StorePath, FullStorePath, NixStoreDir,
                      ioParseFullStorePath)

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
  nixCmd :: NixBinDir -> String -> [String] -> IO t

-- | Run a nix command, using the `getNixBinDir` function
nixCmd' :: NixCmdReturn t => String -> [String] -> IO t
nixCmd' cmd args = getNixBinDir >>= \d -> nixCmd d cmd args

instance NixCmdReturn ByteString where
  nixCmd (NixBinDir nixBin) cmd args = do
    let executable = (nixBin </> ("nix-" <> cmd))
    PB.readProcessWithExitCode executable args "" >>= \case
      (ExitSuccess, stdout, _) -> pure stdout
      (ExitFailure code, _, stderr) -> error $ unlines $ [
          cmd' <> " failed with status " <> show code
          , "STDERR:", B8.unpack stderr]
        where cmd' = "nix-" <> cmd <> " " <> intercalate " " args

instance NixCmdReturn Text where
  nixCmd (NixBinDir nixBin) cmd args = do
    let executable = (nixBin </> ("nix-" <> cmd))
    PT.readProcessWithExitCode executable args "" >>= \case
      (ExitSuccess, stdout, _) -> pure stdout
      (ExitFailure code, _, stderr) -> error $ unlines $ [
          cmd' <> " failed with status " <> show code
          , "STDERR:", unpack stderr]
        where cmd' = "nix-" <> cmd <> " " <> intercalate " " args

instance NixCmdReturn () where
  nixCmd nixBin cmd args = (nixCmd nixBin cmd args :: IO Text) >> pure ()

instance NixCmdReturn FullStorePath where
  nixCmd nixBin cmd args = do
    rawPath <- nixCmd nixBin cmd args
    ioParseFullStorePath rawPath

instance NixCmdReturn StorePath where
  nixCmd nixBin cmd args = do
    (_ :: NixStoreDir, spath) <- nixCmd nixBin cmd args
    pure spath

instance NixCmdReturn [StorePath] where
  nixCmd nixBin cmd args = do
    rawPaths <- nixCmd nixBin cmd args
    forM (T.lines rawPaths) $ \line ->
      snd <$> ioParseFullStorePath line
