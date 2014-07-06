module Velox.Environment where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import System.Directory (canonicalizePath)
import System.FilePath ((</>))

import qualified Codex.Project as CP

import Velox.Project (Project(..))
import Velox.Workspace (Workspace(..), findWorkspace, wsDir)

-- TODO Refactor `Codex` to expose a `findProjects/readProject` function wich return tuples (to avoid type wrap/unwrap here)

data Env = Env { workspace :: Workspace, projects :: [Project], leader :: Maybe Project, isStandalone :: Bool }
  deriving (Eq, Show)

loadEnv :: EitherT String IO Env
loadEnv = do
  (fp, ws) <- liftIO $ do
    fp <- canonicalizePath "."
    ws <- findWorkspace fp
    return (fp, ws)
  case ws of
    Nothing -> do
      prj <- liftIO $ readProject fp
      maybe (left "No cabal project or workspace found.") (right . createStandalone fp) prj
    Just ws -> do
      (prjs, leader) <- liftIO $ do
        prjs <- findProjects $ wsDir ws
        prj  <- readProject fp
        return (prjs, prj)
      right $ Env ws prjs leader False
 where
   createStandalone fp prj = Env (Workspace fp (fp </> ".cabal-sandbox")) [prj] (Just prj) True
   findProjects fp = do
     (CP.Workspace xs) <- CP.getWorkspace fp
     return $ fromWP <$> xs
   readProject fp = do
     p <- CP.readWorkspaceProject fp
     return $ fromWP <$> p
   fromWP (CP.WorkspaceProject id p) = Project p id

