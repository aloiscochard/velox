module Velox.Job.Task where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Either (isLeft)
import Data.Traversable (traverse)
import GHC.IO.Exception (AsyncException)
import System.Process
import System.Exit (ExitCode)

import Velox.Artifact (ArtifactId)
import Velox.Project (ProjectId)

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

data TaskContext = TaskContext
  { asyncs  :: MVar [Async ()] }

newTaskContext :: IO TaskContext
newTaskContext = do
  asyncs  <- newMVar []
  return $ TaskContext asyncs

forkAsync :: TaskContext -> IO a -> IO (Async a)
forkAsync tc fx = do
  asyncs'    <- takeMVar $ asyncs tc
  async <- async fx
  putMVar (asyncs tc) $ (const () <$> async) : asyncs'
  return async

forkProcess :: TaskContext -> IO ProcessHandle -> IO (Async (Either AsyncException ExitCode))
forkProcess tc start = forkAsync tc $ do
  handle <- start
  result <- try $ waitForProcess handle
  when (isLeft result) $ terminateProcess handle
  return result

terminateTaskContext :: TaskContext -> IO ()
terminateTaskContext tc = do
  asyncs'  <- takeMVar $ asyncs tc
  traverse cancel asyncs'
  return ()

