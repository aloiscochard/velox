module Velox.Project where

import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import Distribution.Package (PackageName, pkgName)
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

prjName :: Project -> PackageName
prjName = pkgName . package . packageDescription . prjPkgDesc

prjCabalFile :: Project -> FilePath
prjCabalFile prj =
  prjDir prj </> concat [display $ prjName prj, ".cabal"]

-- TODO Implement proper Velox.Build types
{--
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condExecutables    :: [(String, CondTree ConfVar [Dependency] Executable)],
        condTestSuites     :: [(String, CondTree ConfVar [Dependency] TestSuite)],
        condBenchmarks     :: [(String, CondTree ConfVar [Dependency] Benchmark)]
        --}
prjSrcDirs :: Project -> [FilePath]
prjSrcDirs prj = (prjDir prj </>) <$> (List.nub $ xs ++ ys) where
  xs = maybe [] librarySources $ condLibrary pkg where
    librarySources ct = hsSourceDirs . libBuildInfo . condTreeData $ ct
  ys = hsSourceDirs . buildInfo . condTreeData . snd =<< condExecutables pkg
  pkg = prjPkgDesc prj

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
