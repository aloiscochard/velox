module Velox where

import Data.Attoparsec.Text (parseOnly, parseTest, endOfInput, many')
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine ((<~))
import Control.Concurrent (threadDelay)
import System.Process

import qualified Data.Text as T
import qualified Data.Machine as M

import Codec.GHC.Log (messageParser)

title :: String
title = "velox"

mainVelox :: IO ()
mainVelox = do
  (ec, out, err') <- readCreateProcessWithExitCode (shell "stack build") ""
  -- let err = unlines $ take 23 $ drop (9) $ lines $ err'
  let err = err' -- unlines $ take 6 $ drop (18) $ lines $ err'
  print "ERR:"
  putStrLn err
  print "OUT:"
  putStrLn out

  _ <- traverse print $ M.run $ (streamParser messageParser) <~ (M.source (fmap ((`T.snoc` '\n') . T.pack) $ lines err))
  return ()
