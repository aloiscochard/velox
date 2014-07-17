module Velox.Build where

import Data.Hashable (hash)
import Distribution.Package (Dependency(..), PackageName, pkgName, pkgVersion)
import Distribution.PackageDescription (BuildInfo, hsSourceDirs)

import qualified Distribution.PackageDescription as PD

data BuildId = BuildId BuildKind Int
  deriving (Eq, Ord, Show)

data BuildKind = Library | Executable | TestSuite | Benchmark
  deriving (Eq, Ord, Show)

data Build =
    LibraryBuild    { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildLibrary :: PD.Library }
  | ExecutableBuild { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildExecutable :: PD.Executable }
  | TestSuiteBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildTestSuite :: PD.TestSuite }
  | BenchmarkBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildBenchmark :: PD.Benchmark }

instance Show Build where show = show . buildId

buildId :: Build -> BuildId
buildId x = BuildId (buildKind x) $ hash . hsSourceDirs . buildInfo $ x

buildKind :: Build -> BuildKind
buildKind (LibraryBuild _ _ _) = Library
buildKind (ExecutableBuild _ _ _) = Executable
buildKind (TestSuiteBuild _ _ _) = TestSuite
buildKind (BenchmarkBuild _ _ _) = Benchmark
