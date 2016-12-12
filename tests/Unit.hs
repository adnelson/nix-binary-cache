module Main where

import ClassyPrelude
import Test.Hspec (hspec)


import qualified Nix.Cache.Types.Tests as TypesTests
import qualified Data.KVMap.Tests as KVMapTests
import qualified Data.ByteString.Char8 as B8
import System.IO.Unsafe (unsafePerformIO)

import Nix.StorePath
import Nix.Cache.Client
import Nix.Derivation
import Nix.Derivation.Tests
import Nix.Nar
import Nix.NarExport
import Nix.Bin

-- Define some things purely to suppress redundancy warnings
type FOO1 = StorePath
type FOO2 = NixCacheAuth
type FOO3 = Derivation
type FOO4 = NarExport
foo1 :: NixStoreDir -> Derivation -> IO PathSet
foo1 = derivInputs

spath :: StorePath
spath = StorePath "00782dxdzwfi9306k1f6dj70g8ai8gx5" "python3.4-astroid-1.4.4"

sdir :: NixStoreDir
sdir = unsafePerformIO getNixStoreDir

nbdir :: NixBinDir
nbdir = unsafePerformIO getNixBinDir

export :: NarExport
export = unsafePerformIO $ getExport' spath

exBytes :: ByteString
exBytes = toStrict $ narExportToBytestring export

narBytes :: ByteString
narBytes = narToBytestring $ neNar export

len :: Int
len = length (_EXPORT_MAGIC_1 <> narBytes <> _EXPORT_MAGIC_2)

getn :: Int -> ByteString -> [Int]
getn n = map fromEnum . B8.unpack . take n

rawExport :: ByteString
rawExport = unsafePerformIO $ getExportRaw' spath

main :: IO ()
main = hspec $ do
  TypesTests.nixCacheInfoSpec
  KVMapTests.kvMapSpec
  TypesTests.fileHashSpec
  derivSpec
