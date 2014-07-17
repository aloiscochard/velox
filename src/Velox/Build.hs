module Velox.Build where

import Data.Hashable (hash)
import Distribution.Package (Dependency(..), PackageName, pkgName, pkgVersion)
import Distribution.PackageDescription (BuildInfo, hsSourceDirs)

import qualified Distribution.PackageDescription as PD

data BuildId = BuildId BuildKind Int
  deriving (Eq, Ord, Show)

data BuildKind = Library | Executable | TestSuite | Benchmark
  deriving (Eq, Ord, Show)

bldKindDisplay :: BuildKind -> String
bldKindDisplay Library    = "lib"
bldKindDisplay Executable = "exe"
bldKindDisplay TestSuite  = "test"
bldKindDisplay Benchmark  = "bench"

data Build =
    LibraryBuild    { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldLibrary :: PD.Library }
  | ExecutableBuild { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldExecutable :: PD.Executable }
  | TestSuiteBuild  { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldTestSuite :: PD.TestSuite }
  | BenchmarkBuild  { bldInfo :: BuildInfo, bldDependencies :: [Dependency], bldBenchmark :: PD.Benchmark }

instance Show Build where show = show . bldId

bldId :: Build -> BuildId
bldId x = BuildId (bldKind x) $ hash . hsSourceDirs . bldInfo $ x

bldKind :: Build -> BuildKind
bldKind (LibraryBuild _ _ _) = Library
bldKind (ExecutableBuild _ _ _) = Executable
bldKind (TestSuiteBuild _ _ _) = TestSuite
bldKind (BenchmarkBuild _ _ _) = Benchmark
