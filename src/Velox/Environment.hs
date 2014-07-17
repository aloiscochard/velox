module Velox.Environment where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import Distribution.Text (display)
import GHC.Exts (sortWith)
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath ((</>))

import qualified Data.List as List
import qualified Data.Map.Strict as Map

import Distribution.Sandbox.Utils (findSandbox, readSandboxSources)
import Velox.Build (Build, BuildId, bldId, bldKind)
import Velox.Project (Project(..), ProjectId, findProject, findProjects, prjBuilds, prjId, prjName, resolveReverseDeps)
import Velox.Workspace (Workspace(..), findWorkspace, wsDir)

data Env = Env
  { workspace :: Workspace
  , projects :: [Project]
  , reverseDependencies :: Map (ProjectId, BuildId) [(Project, Build)]
  , leader :: Maybe Project
  , isStandalone :: Bool }

-- TODO Shave that **HAIRY** yak
instance Show Env where
  show (Env ws prjs reverseDeps leader standalone) = unlines $ header:projects where
    header = concat (wsDir ws : if standalone then [" (standalone)"] else [" (workspace)"])
    projects = sortWith (fmap toUpper) $ (\prj -> concat ["\t", display . prjName $ prj, prjReverseDeps prj]) <$> prjs
    prjReverseDeps prj = format reverseDepsDisplay where
      reverseDepsDisplay = List.nub $ List.sort $ fmap (\(prjName, blds) -> concat [prjName, format $ List.nub $ (show . bldKind) <$> blds]) $ Map.toList $ groupByKey xs where
        xs :: [(String, Build)]
        xs = prjBuilds prj >>= (\bld -> fmap (\(x, y) -> (display $ prjName x, y)) $ join $ maybeToList $ Map.lookup (prjId prj, bldId bld) reverseDeps)
      groupByKey :: Ord a => [(a, b)] -> Map a [b]
      groupByKey xs = Map.map (fmap snd) $ groupBy fst xs
      groupBy :: Ord b => (a -> b) -> [a] -> Map b [a]
      groupBy f = Map.fromListWith (++) . map (f &&& return . id)
      format [] = ""
      format xs = concat ["(", List.intercalate ", " xs, ")"]

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
      return . Right $ Env ws (concat [maybeToList leader, prjs]) (resolveReverseDeps prjs) leader False
  where
    createStandalone :: FilePath -> Project -> IO (Either LoadError Env)
    createStandalone fp prj = do
      sandboxPath <- findSandbox fp
      case sandboxPath of
        Nothing   -> return . Left $ NoSandbox fp
        Just p    -> do
          xs <- readSandboxSources p
          ys <- traverse findProject xs
          let prjs = prj:(maybeToList =<< ys)
          return . Right $ Env (Workspace fp p) prjs (resolveReverseDeps prjs) (Just prj) True
      where
        sandboxPath = fp </> ".cabal-sandbox"
