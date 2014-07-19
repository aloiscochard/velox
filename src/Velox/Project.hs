module Velox.Project where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>))
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import Distribution.Package (Dependency(..), PackageName, pkgName, pkgVersion)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (withinRange)
import Safe (headMay)
import System.Directory (canonicalizePath, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import qualified Data.List as List
import qualified Data.Map.Strict as Map

import Velox.Build (Build(..), BuildId, bldId)

newtype ProjectId = ProjectId FilePath
  deriving (Eq, Ord, Show)

data Project = Project { prjDir :: FilePath, prjPkgDesc :: GenericPackageDescription }
  deriving (Eq)

instance Show Project where
  show = display . package . packageDescription . prjPkgDesc

prjId :: Project -> ProjectId
prjId = ProjectId . prjDir

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
    f x = ExecutableBuild (buildInfo (condTreeData x)) (condTreeConstraints x) (condTreeData x)
  testSuites = f . snd <$> condTestSuites pkgDesc where
    f x = TestSuiteBuild (testBuildInfo (condTreeData x)) (condTreeConstraints x) (condTreeData x)
  benchmarks = f . snd <$> condBenchmarks pkgDesc where
    f x = BenchmarkBuild (benchmarkBuildInfo (condTreeData x)) (condTreeConstraints x) (condTreeData x)
  pkgDesc = prjPkgDesc prj

prjSourceDirs :: Project -> Map BuildId [FilePath]
prjSourceDirs prj = Map.fromListWith (++) $ fmap (\x -> (bldId x, (prjDir prj </>) <$> (hsSourceDirs $ bldInfo x))) $ prjBuilds prj

prjSourceModuleFiles :: Project -> Map BuildId [FilePath]
prjSourceModuleFiles prj = Map.fromListWith (++) $ fmap (\x -> (bldId x, (prjDir prj </>) <$> f x)) $ prjBuilds prj where
  f (ExecutableBuild i _ e) = [prefix $ modulePath e] where
    prefix = case headMay $ hsSourceDirs i of
      Just p  -> (p </>)
      Nothing -> id
  f _                       = []


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

resolveReverseDeps :: [Project] -> Map (ProjectId, BuildId) [(Project, Build)]
resolveReverseDeps prjs = List.foldl' f Map.empty prjsWithBuilds where
  f xs (prjA, buildA) = Map.insert (prjId prjA, bldId buildA) reverseDeps xs where
    reverseDeps = do
      prjB <- prjs
      if name prjA == name prjB then []
      else prjBuilds prjB >>= (\buildB -> const (prjB, buildB) <$> (List.filter p $ bldDependencies buildB))
    p (Dependency n vr) = n == name prjA && withinRange (version prjA) vr
  prjsWithBuilds :: [(Project, Build)]
  prjsWithBuilds = prjs >>= (\prj -> (\x -> (prj, x)) <$> prjBuilds prj)
  name = pkgName . pkg
  version = pkgVersion . pkg
  pkg = package . packageDescription . prjPkgDesc
