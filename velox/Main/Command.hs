module Main.Command where

import Velox.Artifact (ArtifactId)
import Velox.Job.Task (Task)
import Velox.Project (ProjectId)

data Command
  = ExecuteTasks [Task]
  | Quit
  deriving (Eq)
