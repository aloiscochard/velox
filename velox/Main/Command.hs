module Main.Command where

import Velox.Artifact (ArtifactId)
import Velox.Project (ProjectId)

data Command =
    Build     ArtifactId [FilePath]
  | Configure ProjectId
  | Quit
  deriving (Eq)
