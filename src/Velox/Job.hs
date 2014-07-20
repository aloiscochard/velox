module Velox.Job where

import Control.Applicative
import Data.Map.Strict (Map)
import Data.Traversable (traverse)
import System.FilePath

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Artifact (ArtifactId(..))
import Velox.Dependencies (Dependencies, directDependencies, filterDependencies, reverseDependencies)
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
  print $ jobTasks j
  putStrLn "(plan)"
  traverse (\xs -> print $ fst <$> xs) xss
  putStrLn "(done)" where
    xss = planJob ds j

