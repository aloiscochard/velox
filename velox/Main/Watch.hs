{-# LANGUAGE Rank2Types #-}
module Main.Watch where

import Control.Applicative ((<$>))
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

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Build (Build, BuildId, bldId, bldInfo)
import Velox.Project (Project, prjDir, prjBuilds, prjSourceDirs)
import Velox.Environment (Env, projects)

data WatchEvent = WatchSource Project BuildId FilePath
                | WatchCabal Project
                deriving (Eq, Show)

withWatch :: Env -> (IOSource WatchEvent -> IO a) -> IO a
withWatch env f = withINotify $ \ino -> do
  events            <- newEmptyMVar
  sourcesWatchers   <- traverse (watchSrc ino events) prjs
  cabalWatchers     <- traverse (watchCabal ino events) prjs
  res <- f $ sourceIO $ takeMVar events
  traverse removeWatch $ L.nub $ (join sourcesWatchers) ++ cabalWatchers
  return res where
    watchSrc ino events prj = do
      xs <- traverse createBuildWatches $ M.toList srcDirs
      return $ join xs where
        createBuildWatches (bldId, fps) = do
          xs <- filterM doesDirectoryExist fps
          ys <- runT $ directories <~ directoryWalk <~ source xs
          traverse createWatch $ (ys ++ xs) where
            createWatch fp = watch ino fp events prj shouldBeWatched (\prj fp -> WatchSource prj bldId fp)
    watchCabal ino events prj =
      watch ino (prjDir prj) events prj (\p -> shouldBeWatched p && L.isSuffixOf ".cabal" p) (\prj fp -> WatchCabal prj)
    watch ino fp events prj p f = addWatch ino varieties fp g where
      g e = case maybeFilePath e of
        Just fp'  -> if p fp' then putMVar events $ f prj (fp </> fp') else return ()
        _         -> return ()
    varieties = [Modify, Move, Create, Delete]
    srcDirs = L.foldl' M.union M.empty $ prjSourceDirs <$> prjs
    prjs = projects env


shouldBeWatched :: FilePath -> Bool
shouldBeWatched fp | L.isPrefixOf "."    fp  = False
shouldBeWatched fp | L.isSuffixOf "~"    fp  = False
shouldBeWatched fp | L.isSuffixOf ".swp" fp  = False
shouldBeWatched fp                              = True

