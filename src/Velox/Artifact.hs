module Velox.Artifact where

import Velox.Build (BuildId, Build, bldId)
import Velox.Project (ProjectId, Project)

import qualified Velox.Project as P

data ArtifactId = ArtifactId ProjectId BuildId
  deriving (Eq, Ord, Show)

data Artifact = Artifact Project Build

artifactId :: Artifact -> ArtifactId
artifactId (Artifact prj bld) = ArtifactId (P.prjId prj) (bldId bld)

prjId :: ArtifactId -> ProjectId
prjId (ArtifactId x _) = x
