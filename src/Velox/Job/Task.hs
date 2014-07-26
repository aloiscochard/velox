{-# LANGUAGE Rank2Types #-}
module Velox.Job.Task where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Exception
import Control.Monad
import Data.Either (isLeft)
import Data.Machine
import Data.Traversable (traverse)
import GHC.IO.Exception (AsyncException)
import System.IO.Machine (IOSink, IOSource, sinkIO, sourceIO)
import System.Process
import System.Exit (ExitCode)

import Velox.Artifact (ArtifactId)
import Velox.Job.Action
import Velox.Project (ProjectId)

import qualified Velox.Display as D

data Task
  = ArtifactTask  ArtifactId  ArtifactAction
  | ProjectTask   ProjectId   ProjectAction
  deriving (Eq, Show)

data TaskContext = TaskContext
  { asyncs        :: MVar [Async ()]
  , displayInput  :: IOSink D.Event }

newTaskContext :: IOSink D.Event -> IO TaskContext
newTaskContext displayHandler = do
  asyncs <- newMVar []
  events <- atomically $ newTQueue
  let tc = TaskContext asyncs (sinkIO (atomically . writeTQueue events))
  forkAsync tc $ runT_ $ displayHandler <~ (prepended [D.JobStart]) <~ (sourceIO . atomically $ readTQueue events)
  return tc

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

