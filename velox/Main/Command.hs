module Main.Command where

import Velox.Build (BuildId)
import Velox.Project (Project)

data Command =
    Build     Project BuildId [FilePath]
  | Configure Project
  | Quit
  deriving (Eq, Show)
