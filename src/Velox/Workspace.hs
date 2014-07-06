module Velox.Workspace where

import System.Directory (doesDirectoryExist)
import System.IO (FilePath)
import System.FilePath ((</>), takeDirectory)

data Workspace = Workspace { wsDir :: FilePath, wsSandboxDir :: FilePath }
  deriving (Eq, Show)

wsDataDir :: FilePath -> FilePath
wsDataDir r = r </> ".velox-workspace"

findWorkspace :: FilePath -> IO (Maybe Workspace)
findWorkspace fp = loadWorkspace fp >>= \ws -> case ws of
  Nothing | takeDirectory fp == fp  -> return Nothing
  Nothing                           -> findWorkspace $ takeDirectory fp
  x                                 -> return x

loadWorkspace :: FilePath -> IO (Maybe Workspace)
loadWorkspace fp = do
  exist <- doesDirectoryExist dataDir
  if exist then do
    return . Just $ Workspace fp (dataDir </> "sandbox")
  else
    return Nothing
  where
    dataDir = wsDataDir fp



