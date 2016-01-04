{-# LANGUAGE TemplateHaskell #-}
module Velox.Plugin (plugin) where

import Neovim
import Velox.Plugin.Internal

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = wrapPlugin Plugin
    { exports         = [ $(function' 'veloxStart) Async ]
    , statefulExports = [] }
