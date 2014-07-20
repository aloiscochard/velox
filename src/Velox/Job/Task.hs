module Velox.Job.Task where

import Control.Concurrent.Async
import Data.Traversable (traverse)
import System.Process (ProcessHandle, terminateProcess)

import Velox.Artifact (ArtifactId)

-- TODO Remove
import Control.Concurrent (threadDelay)

runTask :: (ArtifactId, [FilePath]) -> IO Bool
runTask (a, fps) = do
  putStrLn (show a ++ " START")
  threadDelay $ 1000 * 1000
  putStrLn (show a ++ " FINISH")
  return True

data TaskContext = TaskContext
  { asyncs    :: [Async ()]
  , processes :: [ProcessHandle] }

terminateTaskContext :: TaskContext -> IO ()
terminateTaskContext tc = do
  traverse cancel $ asyncs tc
  traverse terminateProcess $ processes tc
  return ()

