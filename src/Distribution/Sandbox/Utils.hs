module Distribution.Sandbox.Utils where

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (readFile)

readSandboxSources :: FilePath -> IO [FilePath]
readSandboxSources sandboxPath = do
  fileExists  <- doesFileExist sourcesFile
  if fileExists then readSources else return [] where
    readSources = do
      fileContent <- readFile sourcesFile
      return $ projects fileContent where
        projects :: String -> [FilePath]
        projects x = sources x >>= (\x -> fmap fst $ snd x)
        sources :: String -> [(String, [(FilePath, Int)])]
        sources x = read x
    sourcesFile = sandboxPath </> "add-source-timestamps"

