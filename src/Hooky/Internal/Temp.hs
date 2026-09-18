{-# LANGUAGE QuasiQuotes #-}

module Hooky.Internal.Temp (
  hookyTmpDir,
) where

import Control.Monad (forM_, when)
import Data.Time qualified as Time
import Hooky.Utils.OsPath (listDirectoryRecur)
import System.Directory.OsPath (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getModificationTime,
  getXdgDirectory,
  removePathForcibly,
 )
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, osp)

hookyTmpDir :: OsPath
hookyTmpDir = unsafePerformIO getHookyTmpDir
{-# NOINLINE hookyTmpDir #-}

getHookyTmpDir :: IO OsPath
getHookyTmpDir = do
  now <- Time.getCurrentTime
  tmpdir <- getXdgDirectory XdgCache [osp|hooky|]
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
