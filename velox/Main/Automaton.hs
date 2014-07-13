{-# LANGUAGE Rank2Types #-}
module Main.Automaton where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Data.Machine
import Data.Traversable (traverse)
import System.IO (BufferMode(NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.IO.Machine (IOSink, byChar, sinkIO, sourceIO, sourceHandle, printer)
import System.Posix.Signals (Handler(Catch), installHandler, sigINT)

import Velox.Environment (Env)
import Main.Command (Command(..), fromWatchEvent)
import Main.Watch (withWatch)


automaton :: Env -> IO ()
automaton env = withWatch env $ \watchEvents -> do
  commandsVar       <- newEmptyMVar
  let commandsSink  = sinkIO (putMVar commandsVar)

  installHandler sigINT (Catch $ putMVar commandsVar Quit) Nothing

  hSetBuffering stdin NoBuffering
  hSetEcho      stdin False
  keyboardThread    <- forkIO . runT_ $ commandsSink <~ keyboardHandler <~ (sourceHandle byChar stdin)

  watchThread       <- forkIO . runT_ $ commandsSink <~ auto fromWatchEvent <~ watchEvents

  buildVar          <- newEmptyMVar
  runT_ $ commandHandler buildVar <~ sourceIO (takeMVar commandsVar)

  killThread watchThread
  killThread keyboardThread

  buildThread       <- tryTakeMVar buildVar
  traverse killThread buildThread

  return ()

commandHandler :: MVar ThreadId -> IOSink Command
commandHandler buildVar = repeatedly $ await >>= \c -> case c of
  Build prj fps -> do
    liftIO $ do
      tryTakeMVar buildVar >>= traverse killThread
      buildThread <- forkIO $ do
        putStrLn "(start)"
        threadDelay $ 5 * 1000 * 1000
        putStrLn "(stop)"
      putMVar buildVar buildThread
    return ()
  Configure prj -> do
    return ()
  Quit          -> do
    liftIO $ putStrLn "Goodbye."
    stop

keyboardHandler :: Process Char Command
keyboardHandler = repeatedly $ do
  w <- await
  case w of
    '\EOT'  -> yield Quit
    'q'     -> yield Quit
    _       -> return ()
