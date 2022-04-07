{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hooky.Run.ExecutionPlan (
  compilePlan,
  ExecutionPlan (..),
  ExecutionStep (..),
  ExecutionCommand (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Path (File, Rel, Path, toFilePath)
import System.FilePath.Glob qualified as Glob

import Hooky.Config (Check (..), CommandDefinition (..), Config (..))

newtype ExecutionPlan = ExecutionPlan {unExecutionPlan :: [ExecutionStep]}
  deriving (Show, Eq)

data ExecutionStep = ExecutionStep
  { stepName :: Text
  , stepCommand :: ExecutionCommand
  , stepFiles :: NonEmpty (Path Rel File)
  }
  deriving (Show, Eq)

data ExecutionCommand = ExecutionCommand
  { cmdCommand :: Text
  , cmdArgs :: [Text]
  }
  deriving (Show, Eq)

compilePlan :: Config -> [Path Rel File] -> ExecutionPlan
compilePlan Config{..} files =
  ExecutionPlan $
    flip mapMaybe cfgChecks $ \Check{..} -> do
      filesToCheck <- mFilesToCheck
      stepFiles <-
        if null checkFiles
          then Just filesToCheck
          else NonEmpty.nonEmpty $ NonEmpty.filter (matchesAny checkFiles) filesToCheck
      Just
        ExecutionStep
          { stepName = checkName
          , stepCommand =
              case checkCommand of
                ExplicitCommand (cmd NonEmpty.:| args) -> ExecutionCommand cmd args
                CommandFromSource _ -> error "TODO: CommandFromSource" -- TODO
          , stepFiles
          }
  where
    mFilesToCheck = NonEmpty.nonEmpty $ filter (not . matchesAny cfgExclude) files
    matchesAny patterns file = any (`Glob.match` toFilePath file) patterns
