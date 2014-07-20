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
import Velox.Project (prjBuilds)
import Velox.Environment (Env)
import Velox.Job (Job(..), jobTasks, runJob)
import Main.Command (Command)
import Main.Watch (WatchEvent(..), withWatch)

import qualified Main.Command as C

type JobHandle = (ThreadId, Job)

automaton :: Env -> IO ()
automaton env = withWatch env $ \watchEvents -> do
  commandsVar       <- newEmptyMVar
  let commandsSink  = sinkIO (putMVar commandsVar)

  installHandler sigINT (Catch $ putMVar commandsVar C.Quit) Nothing

  hSetBuffering stdin NoBuffering
  hSetEcho      stdin False

  keyboardThread    <- forkIO . runT_ $ commandsSink <~ keyboardHandler <~ (sourceHandle byChar stdin)
  watchThread       <- forkIO . runT_ $ commandsSink <~ watchHandler <~ watchEvents

  jobHandleVar     <- newEmptyMVar
  runT_ $ commandHandler jobHandleVar <~ sourceIO (takeMVar commandsVar)

  killThread watchThread
  killThread keyboardThread

  jobHandle         <- tryTakeMVar jobHandleVar
  traverse (killThread . fst) jobHandle

  return ()


commandHandler :: MVar JobHandle -> IOSink Command
commandHandler jobHandleVar = repeatedly $ await >>= \c -> case c of
  C.Build artifactId' fps -> do
    liftIO $ do
      job <- updateJob
      threadId <- forkIO $ runJob job
      putMVar jobHandleVar (threadId, job)
    return () where
      updateJob = do
        handle <- tryTakeMVar jobHandleVar
        traverse (killThread . fst) handle
        let tasks = M.unionWith (\xs ys -> L.nub $ xs ++ ys) (M.fromList [(artifactId', fps)]) (maybe M.empty (jobTasks . snd) handle)
        return $ Job $ tasks where
  C.Configure prj -> do
    return ()
  C.Quit          -> do
    liftIO $ putStrLn "So long, and thanks for all the fish."
    stop

keyboardHandler :: Process Char Command
keyboardHandler = repeatedly $ do
  w <- await
  case w of
    '\EOT'  -> yield C.Quit
    'q'     -> yield C.Quit
    _       -> return ()

watchHandler :: Process WatchEvent Command
watchHandler = auto f where
  f (WatchSource x fp)  = C.Build x [fp]
  f (WatchCabal  x)     = C.Configure x

