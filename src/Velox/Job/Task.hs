module Velox.Job.Task where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Traversable (traverse)

import Velox.Artifact (ArtifactId)
import Velox.Project (ProjectId)

-- TODO Remove
import Control.Concurrent (threadDelay)

data Task
  = ArtifactTask  ArtifactId  ArtifactAction
  | ProjectTask   ProjectId   ProjectAction
  deriving (Eq, Show)

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

-- TODO Optimize actions!
runArtifactActions :: (ArtifactId, [ArtifactAction]) -> IO Bool
runArtifactActions (a, fps) = do
  putStrLn (show a ++ " START")
  threadDelay $ 1000 * 1000
  putStrLn (show a ++ " FINISH")
  return True

runProjectActions :: (ProjectId, [ProjectAction]) -> IO Bool
runProjectActions = undefined

data TaskContext = TaskContext
  { asyncs      :: MVar [Async ()] }

forkAsync :: TaskContext -> IO a -> IO (Async a)
forkAsync tc fx = do
  asyncs'    <- takeMVar $ asyncs tc
  async <- async fx
  putMVar (asyncs tc) $ (const () <$> async) : asyncs'
  return async

terminateTaskContext :: TaskContext -> IO ()
terminateTaskContext tc = do
  asyncs'  <- takeMVar $ asyncs tc
  traverse cancel asyncs'
  return ()

