module Velox.Build where

import Data.Binary (encode)
import Data.Function (on)
import Data.Hashable (hash)
import Distribution.Package (Dependency(..), PackageName, pkgName, pkgVersion)
import Distribution.PackageDescription (BuildInfo, hsSourceDirs)
import Numeric (showHex)

import qualified Data.ByteString.Lazy as B
import qualified Distribution.PackageDescription as PD

data BuildId = BuildId BuildKind Int
  deriving (Eq, Ord)

instance Show BuildId where
  show (BuildId k i) = concat [show k, "-",  concat . map (flip showHex "") . B.unpack $ encode i]


data BuildKind = Library | Executable | TestSuite | Benchmark
  deriving (Eq, Ord)

instance Show BuildKind where
  show Library    = "lib"
  show Executable = "exe"
  show TestSuite  = "test"
  show Benchmark  = "bench"

data Build =
    LibraryBuild    { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldLibrary :: PD.Library }
  | ExecutableBuild { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldExecutable :: PD.Executable }
  | TestSuiteBuild  { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldTestSuite :: PD.TestSuite }
  | BenchmarkBuild  { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldBenchmark :: PD.Benchmark }
  deriving (Show)

instance Eq Build where
  (==) = (==) `on` bldId

instance Ord Build where
  compare = compare `on` bldId

bldId :: Build -> BuildId
bldId x = BuildId (bldKind x) $ hash . hsSourceDirs . bldInfo $ x

bldKind :: Build -> BuildKind
bldKind (LibraryBuild _ _ _) = Library
bldKind (ExecutableBuild _ _ _) = Executable
bldKind (TestSuiteBuild _ _ _) = TestSuite
bldKind (BenchmarkBuild _ _ _) = Benchmark
