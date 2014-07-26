{-# LANGUAGE Rank2Types #-}
module Main.Watch where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad (filterM, join)
import Data.Machine
import Data.Traversable (traverse)
import Distribution.PackageDescription (hsSourceDirs)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.Directory.Machine (directories, directoryWalk)
import System.IO.Machine (IOSource, sourceIO)
import System.INotify
import System.FilePath ((</>), dropFileName, takeFileName)

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Artifact (ArtifactId(..))
import Velox.Build (bldId, bldInfo)
import Velox.Project (ProjectId, prjId, prjDir, prjBuilds, prjSourceDirs, prjSourceModuleFiles)
import Velox.Environment (Env, projects)

-- TODO Rename to `Event` and use qualified import
data WatchEvent = WatchSource ArtifactId FilePath
                | WatchCabal  ProjectId
                deriving (Eq)

withWatch :: Env -> (IOSource WatchEvent -> IO a) -> IO a
withWatch env f = withINotify $ \ino -> do
  events            <- newEmptyMVar
  sourcesWatchers   <- traverse (watchSrc ino events) prjs
  cabalWatchers     <- traverse (watchCabal ino events) prjs
  res <- f $ sourceIO $ takeMVar events
  traverse removeWatch $ L.nub $ (join sourcesWatchers) ++ cabalWatchers
  return res where
    watchSrc ino events prj = do
      xs <- M.traverseWithKey createBuildWatches $ prjSourceDirs prj
      return $ join $ M.elems xs where
        createBuildWatches bldId' srcDirs = do
          dirs <- findDirs
          dirWatches <- traverse (createWatch shouldBeWatched) dirs
          files <- findFiles
          fileWatches <- traverse (\fp -> createWatch (== takeFileName fp) $ dropFileName fp) files
          return (dirWatches ++ fileWatches) where
            createWatch p fp =  do
              watch ino fp events prj p (\prjId' fp -> WatchSource (ArtifactId prjId' bldId') fp)
            canonicalize = traverse canonicalizePath
            normalizeDirs xs = filterM doesDirectoryExist xs >>= canonicalize
            normalizeFiles xs = filterM doesFileExist xs >>= canonicalize
            findDirs = do
              xs <- normalizeDirs srcDirs
              ys <- runT $ directories <~ directoryWalk <~ source xs
              return (xs ++ ys)
            findFiles = do
              moduleFiles <- normalizeFiles $ M.findWithDefault [] bldId' $ prjSourceModuleFiles prj
              return $ filter (\p -> not $ any (flip L.isPrefixOf p) srcDirs) moduleFiles

    watchCabal ino events prj =
      watch ino (prjDir prj) events prj (\p -> shouldBeWatched p && L.isSuffixOf ".cabal" p) (\prjId' _ -> WatchCabal prjId')
    watch ino fp events prj p f = addWatch ino varieties fp g where
      g e = case maybeFilePath e of
        Just fp'  -> if p fp' then putMVar events $ f (prjId prj) (fp </> fp') else return ()
        _         -> return ()
    varieties = [Modify, Move, Create, Delete]
    prjs = projects env


shouldBeWatched :: FilePath -> Bool
shouldBeWatched fp | L.isPrefixOf "."    fp  = False
shouldBeWatched fp | L.isSuffixOf "~"    fp  = False
shouldBeWatched fp | L.isSuffixOf ".swp" fp  = False
shouldBeWatched fp                              = True

