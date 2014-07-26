{-# LANGUAGE Rank2Types #-}
module Velox.Job where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Traversable (traverse)
import GHC.IO.Exception (AsyncException)
import System.FilePath
import System.IO.Machine (IOSink)

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Artifact (ArtifactId(..))
import Velox.Dependencies
import Velox.Job.Task
import Velox.Project (ProjectId)

import qualified Velox.Artifact as A
import qualified Velox.Display as D

-- TODO Remove
import Control.Concurrent (threadDelay)

data Job = Job { jobTasks :: [Task] }

data Plan = Plan
  { planProjects  :: Map ProjectId  [ProjectAction]
  , planArtifacts :: [[(ArtifactId, [ArtifactAction])]] }

planifyArtifacts :: Dependencies -> [ArtifactId] -> [[ArtifactId]]
planifyArtifacts ds [] = []
planifyArtifacts ds xs = case zs of
    ([], [])    -> if null $ fst ys then [snd ys] else [snd ys, fst ys]
    (fzs, szs)  -> snd ys : (planifyArtifacts ds $ fst ys)
  where
    ys = f ds xs
    zs = f ds $ fst ys
    f ds xs = if null $ snd ys then ([], []) else ys where
      ys = L.break (\a -> null $ M.findWithDefault [] a directDeps) xs
      directDeps = directDependencies deps
      deps = filterDependencies xs ds

planifyJob :: Dependencies -> Job -> Plan
planifyJob ds job = Plan projectActions $ f <$> planifyArtifacts ds artifactIds where
    f = (\xs -> (\x -> (x, M.findWithDefault [] x artifactActions)) <$> xs)
    artifactIds = resolve $ M.keys artifactActions where
      resolve xs  = if zs == xs then xs else resolve zs where
        zs = L.nub (ys ++ xs)
        ys = (\x -> M.findWithDefault [] x reverseDeps) =<< xs
      reverseDeps = reverseDependencies ds
    artifactActions = M.fromListWith (++) xs where
      xs = jobTasks job >>= \task -> case task of
        ArtifactTask i a  -> [(i, [a])]
        _                 -> []
    projectActions = M.fromListWith (++) xs where
      xs = jobTasks job >>= \task -> case task of
        ProjectTask i a   -> [(i, [a])]
        _                 -> []

runPlan :: TaskContext -> Plan -> IO Bool
runPlan tc plan = case plan of
    Plan prjActions [] ->
      traverseWithContext runProjectActions $ M.toList prjActions
    Plan prjActions (xs:xss) -> do
      let (prjActions', remainings) = M.partitionWithKey (p xs) prjActions
      traverseWithContext runProjectActions $ M.toList prjActions'
      traverseWithContext runArtifactActions xs
      runPlan tc (Plan remainings xss) where
        p xs prjId _ = any (\(artId, _) -> A.prjId artId == prjId) xs
  where
    traverseWithContext fx xs = do
      asyncs <- traverse (forkAsync tc . fx) xs
      xs <- traverse wait asyncs
      return $ and xs
    runArtifactActions :: (ArtifactId, [ArtifactAction]) -> IO Bool
    runArtifactActions (a, fps) = do
      -- TODO Optimize actions!
      putStrLn (show a ++ " START")
      threadDelay $ 1000 * 1000
      putStrLn (show a ++ " FINISH")
      return True
    runProjectActions :: (ProjectId, [ProjectAction]) -> IO Bool
    runProjectActions xs = return True

runJob :: Dependencies -> IOSink D.Event -> Job -> IO ()
runJob ds displayHandler j = do
  putStrLn "(start)"
  tc  <- newTaskContext displayHandler
  x   <- tryRun tc
  case x of
    Left _ -> do
      terminateTaskContext tc
      putStrLn $ "(aborted)"
    _                 -> return ()
  where
    tryRun :: TaskContext -> IO (Either AsyncException ())
    tryRun tc = try $ do
      success <- runPlan tc $ planifyJob ds j
      putStrLn $ "(finish) " ++ show success where
