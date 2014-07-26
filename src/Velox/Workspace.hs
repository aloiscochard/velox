module Velox.Workspace where

import Data.Traversable
import System.Directory (canonicalizePath, createDirectory, doesDirectoryExist, removeDirectory)
import System.IO (FilePath)
import System.FilePath ((</>), takeDirectory)
import System.Process (runCommand, waitForProcess)

data Workspace = Workspace { wsDir :: FilePath, wsSandboxDir :: FilePath }
  deriving (Eq, Show)

wsDataDir :: FilePath -> FilePath
wsDataDir r = r </> ".velox-workspace"

initWorkspace :: IO (Maybe Workspace)
initWorkspace = do
  fp <- canonicalizePath "."
  let dataDir = wsDataDir fp
  exist <- doesDirectoryExist dataDir
  if exist then return Nothing else do
    createDirectory dataDir
    wp <- loadWorkspace fp
    traverse (init dataDir) wp
    return wp where
      init dataDir wp = do
        handle <- runCommand $ concat ["cabal sandbox init --sandbox='", dataDir, "'"]
        waitForProcess handle
        return ()

deleteWorkspace :: IO (Maybe FilePath)
deleteWorkspace = do
  fp <- canonicalizePath "."
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
