module Main where

import Velox (getWorkspaceSockAddr)

main :: IO ()
main = do
  sock <- getWorkspaceSockAddr "."
  print sock

{--
import Velox.Client (mainVelox)

main :: IO ()
main = mainVelox
--}
