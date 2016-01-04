module Velox.Plugin.Internal where

import Control.Concurrent (threadDelay)

import Neovim
import Neovim.Quickfix

veloxStart :: Neovim' ()
veloxStart = do
  setqflist qfs New
  liftIO $ threadDelay $ 5 * 1000 * 1000
  setqflist qfs' New
    where
  --qfs = [quickfixListItem (Left 1) (Left 1)]
  qfs = [quickfixListItem (Right "foo.hs") (Left 1)]
  qfs' = [quickfixListItem (Right "bar.hs") (Left 1)]
