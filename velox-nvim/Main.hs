module Main where

import Neovim
import qualified Velox.Plugin as Velox

main :: IO ()
main = neovim defaultConfig
    { plugins = [ Velox.plugin ] }
