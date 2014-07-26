module Velox.Internal where

import System.Exit (ExitCode)
import System.Process (runProcess, waitForProcess)

runCommandIn :: FilePath -> String -> [String] -> IO ExitCode
runCommandIn workingDirectory fp args = do
  handle <- runProcess fp args (Just workingDirectory) Nothing Nothing Nothing Nothing
  waitForProcess handle


