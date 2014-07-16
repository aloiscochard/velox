module Main.Command where

import Velox.Project (Project)

data Command =
    Build     Project   [FilePath]
  | Configure Project
  | Quit
  deriving (Eq, Show)
