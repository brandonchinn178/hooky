module Hooky.Utils.Directory (
  PathType (..),
  getPathType,
  listDirectoryRecur,
) where

import Control.Monad (forM)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
import System.FilePath ((</>))

data PathType = PathType_File | PathType_Dir

getPathType :: FilePath -> IO (Maybe PathType)
getPathType fp = (fmap . fmap) fst . findM (($ fp) . snd) $ pathTypePreds
 where
  findM f = \case
    [] -> pure Nothing
    x : xs -> do
      p <- f x
      if p then pure (Just x) else findM f xs

pathTypePreds :: [(PathType, FilePath -> IO Bool)]
pathTypePreds =
  [ (PathType_File, doesFileExist)
  , (PathType_Dir, doesDirectoryExist)
  ]

listDirectoryRecur :: FilePath -> IO [FilePath]
listDirectoryRecur dir = do
  names <- listDirectory dir
  fmap concat . forM names $ \name -> do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then listDirectoryRecur path
      else pure [path]
