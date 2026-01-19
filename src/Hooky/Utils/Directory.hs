module Hooky.Utils.Directory (
  listDirectoryRecur,
) where

import Control.Monad (forM)
import System.Directory (
  doesDirectoryExist,
  listDirectory,
 )
import System.FilePath ((</>))

listDirectoryRecur :: FilePath -> IO [FilePath]
listDirectoryRecur dir = do
  names <- listDirectory dir
  fmap concat . forM names $ \name -> do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then listDirectoryRecur path
      else pure [path]
