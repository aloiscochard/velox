module Velox.Display where

import Velox.Artifact (ArtifactId)
import Velox.Project (ProjectId)
import Velox.Job.Action

data Event
  = JobStart
  | ProjectActionsStart  [(ProjectId, [ProjectAction])]
  | ArtifactActionsStart [(ArtifactId, [ArtifactAction])]
  | Info String
  | Finish Bool
  | Aborted
  deriving (Show)

