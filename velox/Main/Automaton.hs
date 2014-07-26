{-# LANGUAGE Rank2Types #-}
module Main.Automaton where

import Control.Applicative
import Control.Monad
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (maybeToList)
import Data.Machine
import Data.Traversable (traverse)
import System.IO (BufferMode(NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.IO.Machine (IOSink, byChar, sinkIO, sourceIO, sourceHandle, printer)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT)

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Artifact (Artifact(..), artifactId)
import Velox.Build (bldId)
import Velox.Dependencies (Dependencies)
import Velox.Project (prjBuilds)
import Velox.Environment (Env, dependencies)
import Velox.Job (Job(..), jobTasks, runJob)
import Velox.Job.Task (Task(..), ArtifactAction(..), ProjectAction(..))
import Main.Command (Command(..))
import Main.Watch (WatchEvent(..), withWatch)

type JobHandle = (ThreadId, Job)

automaton :: Env -> IO ()
automaton env = withWatch env $ \watchEvents -> do
  commandsVar       <- newEmptyMVar
  let commandsSink  = sinkIO (putMVar commandsVar)

  installHandler sigINT (Catch $ putMVar commandsVar Quit) Nothing

  hSetBuffering stdin NoBuffering
  hSetEcho      stdin False

  keyboardThread    <- forkIO . runT_ $ commandsSink <~ keyboardHandler <~ (sourceHandle byChar stdin)
  watchThread       <- forkIO . runT_ $ commandsSink <~ watchHandler <~ watchEvents

  jobHandleVar     <- newEmptyMVar
  runT_ $ commandHandler (dependencies env) jobHandleVar <~ sourceIO (takeMVar commandsVar)

  killThread watchThread
  killThread keyboardThread

  jobHandle         <- tryTakeMVar jobHandleVar
  traverse (killThread . fst) jobHandle

  return ()


commandHandler :: Dependencies -> MVar JobHandle -> IOSink Command
commandHandler ds jobHandleVar = repeatedly $ await >>= \c -> case c of
  ExecuteTasks tasks -> do
    liftIO $ do
      job <- updateJob
      threadId <- forkIO $ do
        runJob ds job
        tryTakeMVar jobHandleVar
        return ()
      putMVar jobHandleVar (threadId, job)
    return () where
      updateJob = do
        handle <- tryTakeMVar jobHandleVar
        traverse (killThread . fst) handle
        return $ Job $ (join $ maybeToList $ (jobTasks . snd) <$> handle) ++ tasks where
  Quit          -> do
    liftIO $ putStrLn "So long, and thanks for all the fish."
    stop

keyboardHandler :: Process Char Command
keyboardHandler = repeatedly $ do
  w <- await
  case w of
    '\EOT'  -> yield Quit
    'q'     -> yield Quit
    _       -> return ()

watchHandler :: Process WatchEvent Command
watchHandler = auto f where
  f (WatchSource x fp)  = ExecuteTasks [ArtifactTask x $ TypeCheck [fp], ArtifactTask x Build]
  f (WatchCabal  x)     = ExecuteTasks [ProjectTask x Configure]

