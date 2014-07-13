module Main.Command where

import Velox.Project (Project)
import Main.Watch (WatchEvent(..))

data Command =
    Build     Project   [FilePath]
  | Configure Project
  | Quit
  deriving (Eq, Show)

fromWatchEvent :: WatchEvent -> Command
fromWatchEvent (WatchSource prj fp) = Build prj [fp]
fromWatchEvent (WatchCabal  prj)    = Configure prj
