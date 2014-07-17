{-# LANGUAGE Rank2Types #-}
module Main.Watch where

import Control.Concurrent.MVar
import Control.Monad (filterM, join)
import Data.Machine
import Data.Traversable (traverse)
import Distribution.PackageDescription (hsSourceDirs)
import System.Directory (doesDirectoryExist)
import System.Directory.Machine (directories, directoryWalk)
import System.IO.Machine (IOSource, sourceIO)
import System.INotify
import System.FilePath ((</>))

import qualified Data.List as List

import Velox.Build (buildInfo)
import Velox.Project (Project, prjDir, prjBuilds)
import Velox.Environment (Env, projects)

data WatchEvent = WatchSource Project FilePath
                | WatchCabal Project
                deriving (Eq, Show)

withWatch :: Env -> (IOSource WatchEvent -> IO a) -> IO a
withWatch env f = withINotify $ \ino -> do
  events            <- newEmptyMVar
  sourcesWatchers   <- traverse (watchSrc ino events) prjs
  cabalWatchers     <- traverse (watchCabal ino events) prjs
  res <- f $ sourceIO $ takeMVar events
  traverse removeWatch $ List.nub $ (join sourcesWatchers) ++ cabalWatchers
  return res where
    watchSrc ino events prj = do
      xs <- filterM doesDirectoryExist $ prjBuilds prj >>= hsSourceDirs . buildInfo
      ys <- runT $ directories <~ directoryWalk <~ source xs
      traverse createWatch $ (ys ++ xs) where
        createWatch fp = watch ino fp events prj shouldBeWatched WatchSource
    watchCabal ino events prj =
      watch ino (prjDir prj) events prj (\p -> shouldBeWatched p && List.isSuffixOf ".cabal" p) (\prj fp -> WatchCabal prj)
    watch ino fp events prj p f = addWatch ino varieties fp g where
      g e = case maybeFilePath e of
        Just fp'  -> if p fp' then putMVar events $ f prj (fp </> fp') else return ()
        _         -> return ()
    varieties = [Modify, Move, Create, Delete]
    prjs = projects env

shouldBeWatched :: FilePath -> Bool
shouldBeWatched fp | List.isPrefixOf "."    fp  = False
shouldBeWatched fp | List.isSuffixOf "~"    fp  = False
shouldBeWatched fp | List.isSuffixOf ".swp" fp  = False
shouldBeWatched fp                              = True

