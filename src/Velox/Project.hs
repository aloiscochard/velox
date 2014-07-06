module Velox.Project where

import Distribution.Package (PackageIdentifier)
import Distribution.Text (display)

data Project = Project { prjDir :: FilePath, prjId :: PackageIdentifier }
  deriving (Eq)

instance Show Project where
  show = display . prjId
