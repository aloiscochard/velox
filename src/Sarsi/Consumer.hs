{-# LANGUAGE Rank2Types #-}
module Sarsi.Consumer where

import Codec.Velox (Event)

import System.IO.Machine (IOSink)

consume :: IOSink Event -> IO ()
consume = undefined
