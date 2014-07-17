module Main where

import Distribution.Text (display)
import Paths_velox (version)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

import Velox.Environment (LoadError(..), loadEnv)
import Main.Automaton (automaton)

main :: IO ()
main = do
  args  <- getArgs
  run args where
    run ["--version"] = putStrLn $ concat ["velox: ", display version]
    run _             = do
      env <- loadEnv
      case env of
        Left NoAnchor       -> error "No cabal project or workspace found."
        Left (NoSandbox p)  -> error $ concat ["No sandbox exists at'", p, "'."]
        Right env           -> automaton env
      return () where
        error msg = do
          putStrLn $ concat ["velox: ", msg]
          exitWith (ExitFailure 1)

