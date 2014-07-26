module Velox.Workspace where

import System.Directory (canonicalizePath, createDirectory, doesDirectoryExist, removeDirectory)
import System.IO (FilePath)
import System.FilePath ((</>), takeDirectory)

data Workspace = Workspace { wsDir :: FilePath, wsSandboxDir :: FilePath }
  deriving (Eq, Show)

wsDataDir :: FilePath -> FilePath
wsDataDir r = r </> ".velox-workspace"

initWorkspace :: FilePath -> IO (Maybe Workspace)
initWorkspace fp' = do
  fp <- canonicalizePath fp'
  let dataDir = wsDataDir fp
  exist <- doesDirectoryExist dataDir
  if exist then return Nothing else do
    createDirectory dataDir
    -- TODO Run `cabal sandbox init`
    loadWorkspace fp

deleteWorkspace :: FilePath -> IO (Maybe FilePath)
deleteWorkspace fp' = do
  fp <- canonicalizePath fp'
  let dataDir = wsDataDir fp
  exist <- doesDirectoryExist dataDir
  if exist then do
    removeDirectory dataDir
    return $ Just dataDir
  else return Nothing

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



