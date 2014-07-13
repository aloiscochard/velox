module Velox.Environment where

import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath ((</>))

import Distribution.Sandbox.Utils (readSandboxSources)
import Velox.Project (Project(..), findProject, findProjects)
import Velox.Workspace (Workspace(..), findWorkspace, wsDir)

data Env = Env { workspace :: Workspace, projects :: [Project], leader :: Maybe Project, isStandalone :: Bool }
  deriving (Eq, Show)

data LoadError = NoAnchor | NoSandbox FilePath
  deriving (Eq, Show)

loadEnv :: IO (Either LoadError Env)
loadEnv = do
  fp <- canonicalizePath "."
  ws <- findWorkspace fp
  case ws of
    Nothing -> do
      prjDesc <- findProject fp
      case prjDesc of
        Nothing      -> return $ Left NoAnchor
        Just prjDesc -> do
          prj <- createStandalone fp prjDesc
          return prj
    Just ws -> do
      prjs <- findProjects $ wsDir ws
      leader <- findProject fp
      return . Right $ Env ws (concat [maybeToList leader, prjs]) leader False
  where
    createStandalone :: FilePath -> Project -> IO (Either LoadError Env)
    createStandalone fp prj = do
      sandboxExists <- doesDirectoryExist sandboxPath
      if not sandboxExists then
        return . Left $ NoSandbox sandboxPath
      else do
        xs <- readSandboxSources sandboxPath
        ys <- traverse findProject xs
        return . Right $ Env (Workspace fp (sandboxPath)) (prj:(maybeToList =<< ys)) (Just prj) True
      where
        sandboxPath = fp </> ".cabal-sandbox"



