module Velox.Job where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Traversable (traverse)
import GHC.IO.Exception (AsyncException(ThreadKilled))
import System.FilePath

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Artifact (ArtifactId(..))
import Velox.Dependencies (Dependencies, directDependencies, filterDependencies, reverseDependencies)
import Velox.Job.Task (TaskContext(..), runTask, terminateTaskContext)
import Velox.Project (prjId)

data Job = Job { jobTasks :: Map ArtifactId [FilePath] }

planArtifacts :: Dependencies -> [ArtifactId] -> [[ArtifactId]]
planArtifacts ds [] = []
planArtifacts ds xs = case zs of
    ([], [])    -> if null $ fst ys then [snd ys] else [snd ys, fst ys]
    (fzs, szs)  -> snd ys : (planArtifacts ds $ fst ys)
  where
    ys = f ds xs
    zs = f ds $ fst ys
    f ds xs = if null $ snd ys then ([], []) else ys where
      ys = L.break (\a -> null $ M.findWithDefault [] a directDeps) xs
      directDeps = directDependencies deps
      deps = filterDependencies xs ds

planJob :: Dependencies -> Job ->  [[(ArtifactId, [FilePath])]]
planJob ds job = f <$> planArtifacts ds artifactIds where
    f = (\xs -> (\x -> (x, M.findWithDefault [] x tasks)) <$> xs)
    artifactIds = resolve $ M.keys tasks where
      resolve xs  = if zs == xs then xs else resolve zs where
        zs = L.nub (ys ++ xs)
        ys = (\x -> M.findWithDefault [] x reverseDeps) =<< xs
      reverseDeps = reverseDependencies ds
    tasks       = jobTasks job

runJob :: Dependencies -> Job -> IO ()
runJob ds j = do
  putStrLn "(start)"
  tcv <- newMVar $ TaskContext [] []
  res <- tryRun tcv
  case res of
    Left ThreadKilled -> do
      tc  <- takeMVar tcv
      terminateTaskContext tc
      putStrLn $ "(aborted)"
    _                 -> return ()
  where
    tryRun :: MVar TaskContext -> IO (Either AsyncException ())
    tryRun tcv = try $ do
      success         <- foldM (runStep tcv) True $ planJob ds j
      putStrLn $ "(finish) " ++ show success where
        runStep tcv False xs = return False
        runStep tcv True  xs = do
          asyncs <- traverse (createAsync tcv . runTask) xs
          xs <- traverse wait asyncs
          return $ and xs where
            createAsync :: MVar TaskContext -> IO a -> IO (Async a)
            createAsync v fx = do
              tc    <- takeMVar v
              async <- async fx
              putMVar v $ tc { asyncs = (const () <$> async) : asyncs tc }
              return async

