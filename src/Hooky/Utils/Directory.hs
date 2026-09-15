{-# LANGUAGE MultiWayIf #-}

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
import System.IO.Unsafe (unsafeInterleaveIO)

data PathType = PathType_File | PathType_Dir

getPathType :: FilePath -> IO (Maybe PathType)
getPathType fp = do
  isFile <- unsafeInterleaveIO $ doesFileExist fp
  isDir <- unsafeInterleaveIO $ doesDirectoryExist fp
  if
    | isFile -> pure $ Just PathType_File
    | isDir -> pure $ Just PathType_Dir
    | otherwise -> pure Nothing

listDirectoryRecur :: FilePath -> IO [FilePath]
listDirectoryRecur dir = do
  names <- listDirectory dir
  fmap concat . forM names $ \name -> do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then listDirectoryRecur path
      else pure [path]
