module Main where

import Control.Monad.Trans.Either (runEitherT)
import Distribution.Text (display)
import Paths_velox (version)

import Velox.Environment (loadEnv)

main :: IO ()
main = do
  putStrLn $ concat ["velox: ", display version]
  x <- runEitherT $ loadEnv
  print x
  return ()
