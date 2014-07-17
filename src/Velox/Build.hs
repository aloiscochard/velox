module Velox.Build where

import Distribution.Package (Dependency(..), PackageName, pkgName, pkgVersion)
import Distribution.PackageDescription (Benchmark, BuildInfo, Executable, Library, TestSuite, hsSourceDirs)

newtype BuildId = BuildId [FilePath]
  deriving (Eq, Ord, Show)

data Build =
    LibraryBuild    { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildLibrary :: Library }
  | ExecutableBuild { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildExecutable :: Executable }
  | TestSuiteBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildTestSuite :: TestSuite }
  | BenchmarkBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildBenchmark :: Benchmark }

buildId :: Build -> BuildId
buildId = BuildId . hsSourceDirs . buildInfo
