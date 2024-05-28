module FindFile (findFirstFileWithExtension) where

import Data.List (find)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

-- Find the first file with the given extension in the specified directory
findFirstFileWithExtension :: FilePath -> String -> IO (Maybe FilePath)
findFirstFileWithExtension dir ext = do
  allPaths <- listDirectory dir
  let files = map (dir </>) allPaths
  return $ find ((== ext) . takeExtension) files

