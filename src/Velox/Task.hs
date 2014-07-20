module Velox.Task where

import Control.Applicative
import Data.Map.Strict (Map)
import Data.Traversable (traverse)
import System.FilePath

import qualified Data.Map.Strict as M

import Velox.Artifact (ArtifactId)

-- TODO REMOVE
import Control.Concurrent (threadDelay)

data Task = Task { taskActions :: Map ArtifactId [FilePath] }

runTask :: Task -> IO ()
runTask (Task actions) = do
  print actions
  threadDelay $ 5 * 1000 * 1000
  putStrLn "(done)"

