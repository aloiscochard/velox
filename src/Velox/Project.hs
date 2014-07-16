module Velox.Project where

import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import Distribution.Package (Dependency, PackageName, pkgName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import System.Directory (canonicalizePath, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import qualified Data.List as List

data Project = Project { prjDir :: FilePath, prjPkgDesc :: GenericPackageDescription }
  deriving (Eq)

instance Show Project where
  show = display . package . packageDescription . prjPkgDesc

data Build =
    LibraryBuild    { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildLibrary :: Library }
  | ExecutableBuild { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildExecutable :: Executable }
  | TestSuiteBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildTestSuite :: TestSuite }
  | BenchmarkBuild  { buildInfo :: BuildInfo, buildDependencies :: [Dependency], buildBenchmark :: Benchmark }

prjName :: Project -> PackageName
prjName = pkgName . package . packageDescription . prjPkgDesc

prjCabalFile :: Project -> FilePath
prjCabalFile prj =
  prjDir prj </> concat [display $ prjName prj, ".cabal"]

prjBuilds :: Project -> [Build]
prjBuilds prj = maybeToList library ++ executables ++ testSuites where
  library = f <$> condLibrary pkgDesc where
    f x = LibraryBuild (libBuildInfo (condTreeData x)) (condTreeConstraints x) (condTreeData x)
  executables = f . snd <$> condExecutables pkgDesc where
    f x = ExecutableBuild (execBuildInfo (condTreeData x)) (condTreeConstraints x) (condTreeData x)
  testSuites = f . snd <$> condTestSuites pkgDesc where
    f x = TestSuiteBuild (testBuildInfo (condTreeData x)) (condTreeConstraints x) (condTreeData x)
  benchmarks = f . snd <$> condBenchmarks pkgDesc where
    f x = BenchmarkBuild (benchmarkBuildInfo (condTreeData x)) (condTreeConstraints x) (condTreeData x)
  pkgDesc = prjPkgDesc prj
  execBuildInfo = Distribution.PackageDescription.buildInfo

findProject :: FilePath -> IO (Maybe Project)
findProject root = do
  files <- getDirectoryContents root
  traverse readProject $ List.find (List.isSuffixOf ".cabal") files where
    readProject p = Project root <$> readPackageDescription silent (root </> p) where

findProjects :: FilePath -> IO [Project]
findProjects root' = do
  root <- canonicalizePath root'
  xs <- listDirectory root
  ys <- traverse find xs
  return $ ys >>= maybeToList where
    find path = do
      isDirectory <- doesDirectoryExist path
      if isDirectory then findProject path else return Nothing
    listDirectory fp = do
      xs <- getDirectoryContents fp
      return . fmap (fp </>) $ filter (not . List.isPrefixOf ".") xs
