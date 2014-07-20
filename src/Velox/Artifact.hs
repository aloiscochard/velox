module Velox.Artifact where

import Velox.Build (BuildId, Build, bldId)
import Velox.Project (ProjectId, Project, prjId)

data ArtifactId = ArtifactId ProjectId BuildId
  deriving (Eq, Ord, Show)

data Artifact = Artifact Project Build

artifactId :: Artifact -> ArtifactId
artifactId (Artifact prj bld) = ArtifactId (prjId prj) (bldId bld)
