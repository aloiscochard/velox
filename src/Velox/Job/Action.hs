module Velox.Job.Action where

data ArtifactAction
  = TypeCheck [FilePath]
  | Build
  deriving (Eq, Ord, Show)

data ProjectAction
  = Configure
  deriving (Eq, Ord, Show)

data Action
  = Success
  | Failure String

