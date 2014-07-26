module Main where

import Distribution.Text (display)
import Paths_velox (version)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

import Velox.Environment (LoadError(..), loadEnv)
import Velox.Workspace (initWorkspace, deleteWorkspace, wsDir, wsDataDir)
import Main.Automaton (automaton)

main :: IO ()
main = do
  args  <- getArgs
  run args where
    run ["workspace", "init"] = initWorkspace "." >>= \x -> case x of
      Nothing -> error "Workspace already exists, run `velox workspace delete` first if you want to reinitialize it."
      Just ws -> putStrLn $ concat ["Workspace has been successfully initialized at '", wsDataDir $ wsDir ws, "'."]
    run ["workspace", "delete"] = deleteWorkspace "." >>= \x -> case x of
      Nothing -> error "No workspace found."
      Just fp -> putStrLn $ concat ["Workspace '", fp, "' has been successfully deleted."]
    run ["--version"] = putStrLn $ concat ["velox: ", display version]
    run _             = do
      env <- loadEnv
      case env of
        Left NoAnchor       -> error "No cabal project or workspace found."
        Left (NoSandbox p)  -> error $ concat ["No sandbox exists at'", p, "'."]
        Right env           -> automaton env
      return ()
    error msg = do
      putStrLn msg
      exitWith (ExitFailure 1)

