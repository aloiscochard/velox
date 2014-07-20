module Velox.Dependencies where

import Control.Applicative
import Data.Map.Strict (Map)
import Distribution.Package (Dependency(..), pkgName, pkgVersion)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.Version (withinRange)

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Velox.Artifact (Artifact(..), ArtifactId, artifactId)

import Velox.Build (Build, bldDependencies)
import Velox.Project (Project, prjBuilds, prjPkgDesc)

newtype ReverseDependencies = ReverseDependencies (Map ArtifactId [ArtifactId])

resolveReverseDependencies :: [Project] -> ReverseDependencies
resolveReverseDependencies prjs = ReverseDependencies $ L.foldl' f M.empty prjsWithBuilds where
  f xs (prjA, buildA) = M.insert (artifactId $ Artifact prjA buildA) reverseDeps xs where
    reverseDeps = do
      prjB <- prjs
      if name prjA == name prjB then []
      else prjBuilds prjB >>= (\buildB -> const (artifactId $ Artifact prjB buildB) <$> (L.filter p $ bldDependencies buildB))
    p (Dependency n vr) = n == name prjA && withinRange (version prjA) vr
  prjsWithBuilds :: [(Project, Build)]
  prjsWithBuilds = prjs >>= (\prj -> (\x -> (prj, x)) <$> prjBuilds prj)
  name = pkgName . pkg
  version = pkgVersion . pkg
  pkg = package . packageDescription . prjPkgDesc
