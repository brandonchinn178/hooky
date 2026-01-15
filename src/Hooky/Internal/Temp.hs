module Hooky.Internal.Temp (
  hookyTmpDir,
) where

import Control.Monad (forM_, when)
import Data.Time qualified as Time
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  doesDirectoryExist,
  getModificationTime,
  getXdgDirectory,
  listDirectory,
  removePathForcibly,
 )
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

hookyTmpDir :: FilePath
hookyTmpDir = unsafePerformIO getHookyTmpDir
{-# NOINLINE hookyTmpDir #-}

getHookyTmpDir :: IO FilePath
getHookyTmpDir = do
  now <- Time.getCurrentTime
  tmpdir <- getXdgDirectory XdgCache "hooky"
  createDirectoryIfMissing True tmpdir
  cleanup now tmpdir
  pure tmpdir
 where
  -- clean up all files older than 7 days
  cleanup now dir = do
    paths <- map (dir </>) <$> listDirectory dir
    forM_ paths $ \path -> do
      isDir <- doesDirectoryExist path
      if isDir
        then cleanup now path
        else do
          t <- getModificationTime path
          when (Time.addUTCTime (7 * day) t < now) $ do
            removePathForcibly path
  day = 60 * 60 * 24
