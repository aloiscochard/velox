module Velox.Dependencies where

import Control.Applicative
import Data.Map.Strict (Map)
import Distribution.Package (Dependency(..), pkgName, pkgVersion)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.Version (withinRange)

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Artifact (Artifact(..), ArtifactId(..), artifactId)
import Velox.Build (Build, bldDependencies)
import Velox.Project (Project, ProjectId, prjId, prjBuilds, prjPkgDesc)

import qualified Velox.Artifact as A

data Dependencies = Dependencies
  { directDependencies  :: (Map ArtifactId [Project])
  , reverseDependencies :: (Map ArtifactId [ArtifactId]) }

filterDependencies :: [ArtifactId] -> Dependencies -> Dependencies
filterDependencies artifactIds ds = Dependencies directDeps reverseDeps where
  directDeps  = M.mapWithKey g $ M.map f $ filterKeys $ directDependencies ds where
    g a ps  = L.filter (\x -> prjId x /= A.prjId a) ps
    f       = L.filter (\x -> any ((==) $ prjId x) prjIds)
    prjIds  = A.prjId <$> artifactIds
  reverseDeps = M.map f $ filterKeys $ reverseDependencies ds where
    f = L.filter (\x -> any ((==) x) artifactIds)
  filterKeys  = M.filterWithKey (\k v -> any ((==) k) $ artifactIds)

-- TODO Optimize
resolveDependencies :: [Project] -> Dependencies
resolveDependencies prjs = Dependencies resolve resolveReverse where
  p prj(Dependency n vr) = n == name prj && withinRange (version prj) vr
  resolve = L.foldl' f M.empty prjsWithBuilds where
    f xs (prjA, buildA) = M.insert (artifactId $ Artifact prjA buildA) deps xs where
      deps = (\d -> L.filter (flip p d) prjs) =<< bldDependencies buildA
  resolveReverse = L.foldl' f M.empty prjsWithBuilds where
    f xs (prjA, buildA) = M.insert (artifactId $ Artifact prjA buildA) reverseDeps xs where
      reverseDeps = do
        prjB <- prjs
        if name prjA == name prjB then []
        else prjBuilds prjB >>= (\buildB -> const (artifactId $ Artifact prjB buildB) <$> (L.filter (p prjA) $ bldDependencies buildB))
  prjsWithBuilds :: [(Project, Build)]
  prjsWithBuilds = prjs >>= (\prj -> (\x -> (prj, x)) <$> prjBuilds prj)
  name = pkgName . pkg
  version = pkgVersion . pkg
  pkg = package . packageDescription . prjPkgDesc
