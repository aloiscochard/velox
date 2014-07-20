module Velox.Job where

import Control.Applicative
import Data.Map.Strict (Map)
import Data.Traversable (traverse)
import System.FilePath

import qualified Data.Map.Strict as M

import Velox.Artifact (ArtifactId)

-- TODO REMOVE
import Control.Concurrent (threadDelay)

data Job = Job { jobTasks :: Map ArtifactId [FilePath] }

runJob :: Job -> IO ()
runJob (Job tasks) = do
  print tasks
  threadDelay $ 5 * 1000 * 1000
  putStrLn "(done)"

