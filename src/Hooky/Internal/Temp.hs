module Hooky.Internal.Temp (
  hookyTmpDir,
) where

import Control.Monad (forM_, when)
import Data.Time qualified as Time
import Hooky.Utils.Directory (listDirectoryRecur)
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getModificationTime,
  getXdgDirectory,
  removePathForcibly,
 )
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
    paths <- listDirectoryRecur dir
    forM_ paths $ \path -> do
      t <- getModificationTime path
      when (Time.addUTCTime (7 * day) t < now) $ do
        removePathForcibly path
  day = 60 * 60 * 24
