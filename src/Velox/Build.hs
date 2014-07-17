module Velox.Build where

import Data.Hashable (hash)
import Distribution.Package (Dependency(..), PackageName, pkgName, pkgVersion)
import Distribution.PackageDescription (Benchmark, BuildInfo, Executable, Library, TestSuite, hsSourceDirs)

data BuildId = BuildId BuildKind Int
  deriving (Eq, Ord, Show)

data BuildKind = Lib | Exe | Test | Bench
  deriving (Eq, Ord, Show)

data Build =
    LibraryBuild    { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildLibrary :: Library }
  | ExecutableBuild { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildExecutable :: Executable }
  | TestSuiteBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildTestSuite :: TestSuite }
  | BenchmarkBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildBenchmark :: Benchmark }

instance Show Build where show = show . buildId

buildId :: Build -> BuildId
buildId x = BuildId (buildKind x) $ hash . hsSourceDirs . buildInfo $ x

buildKind :: Build -> BuildKind
buildKind (LibraryBuild _ _ _) = Lib
buildKind (ExecutableBuild _ _ _) = Exe
buildKind (TestSuiteBuild _ _ _) = Test
buildKind (BenchmarkBuild _ _ _) = Bench
