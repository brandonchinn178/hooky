{-# LANGUAGE RecordWildCards #-}

module Hooky.Run (
  doRun,
) where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as ByteStringL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Path (toFilePath)
import System.IO (hFlush, stdout)
import System.Process.Typed (ExitCode (..), proc, readProcessInterleaved)
import Text.Printf (printf)

import Hooky.Config (Config)
import Hooky.Run.ExecutionPlan (
  ExecutionCommand (..),
  ExecutionPlan (..),
  ExecutionStep (..),
  compilePlan,
 )
import Hooky.Utils.Git (GitRepo, getStagedFiles)

doRun :: GitRepo -> Config -> IO ()
doRun repo config = do
  files <- getStagedFiles repo
  let ExecutionPlan plan = compilePlan config files
  forM_ plan $ \ExecutionStep{..} -> do
    -- TODO: More sophisticated terminal output
    printf "=====> Running: \"%s\"... " stepName >> hFlush stdout

    let ExecutionCommand cmd args = stepCommand
    (code, output) <-
      readProcessInterleaved $
        proc
          (Text.unpack cmd)
          (map Text.unpack args <> (map toFilePath . NonEmpty.toList) stepFiles)

    case code of
      ExitSuccess -> putStrLn "PASSED"
      ExitFailure _ -> do
        putStrLn "FAILED"
        ByteStringL.hPutStr stdout output
