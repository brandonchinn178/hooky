{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hooky.Config (
  -- * Config
  Config (..),
  loadConfig,
  parseRepoConfig,

  -- * RepoConfig
  RepoConfig (..),
  HookConfig (..),
  PassFilesMode (..),
  LintRule (..),
  LintRuleRule (..),

  -- * GlobalConfig
  GlobalConfig (..),

  -- ** RunMode
  RunMode (..),
  allRunModes,
  parseRunMode,
  renderRunMode,
) where

import Control.Arrow (returnA)
import Control.Monad (unless)
import Data.Bifunctor qualified as Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Records (HasField (..))
import Hooky.Error (abort)
import Hooky.Internal.Output (OutputFormat (..), parseOutputFormat)
import Hooky.Utils.Glob (Glob, toGlob)
import Hooky.Utils.OsPath qualified as OsPath
import KDL.Arrow qualified as KDL
import System.Directory.OsPath (XdgDirectory (..), doesFileExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.File.OsPath qualified as OsPath
import System.OsPath (OsPath, osp, (</>))

data Config = Config
  { repoConfigPath :: OsPath
  , repo :: RepoConfig
  , global :: GlobalConfig
  , skippedHooks :: Set Text
  }
  deriving (Show, Eq)

loadConfig :: OsPath -> IO Config
loadConfig repoConfigPath = do
  global <- loadGlobalConfig
  repo <- loadRepoConfig repoConfigPath

  let fromCSV = Text.splitOn "," . Text.pack
  skippedHooks <- Set.fromList . maybe [] fromCSV <$> lookupEnv "SKIP"

  pure Config{..}

{----- RepoConfig -----}

data RepoConfig = RepoConfig
  { fileGlobs :: [Glob]
  , hooks :: [HookConfig]
  , lintRules :: [LintRule]
  }
  deriving (Show, Eq)

loadRepoConfig :: OsPath -> IO RepoConfig
loadRepoConfig path = do
  configFileExists <- doesFileExist path
  unless configFileExists $ do
    abort $ "Config file doesn't exist: " <> OsPath.toText path
  content <- Text.decodeUtf8 <$> OsPath.readFile' path
  case parseRepoConfig content of
    Right config -> pure config
    Left e -> abort $ "Could not parse config: " <> OsPath.toText path <> "\n" <> e

parseRepoConfig :: Text -> Either Text RepoConfig
parseRepoConfig = Bifunctor.first KDL.renderDecodeError . KDL.decodeWith decoder
 where
  decoder = KDL.document $ proc () -> do
    mDefaults <- KDL.optional . KDL.nodeWith "defaults" $ KDL.children defaultsDecoder -< ()
    let fileGlobs = getDefault [] mDefaults $ \(x) -> x
    hooks <- KDL.many $ KDL.node "hook" -< ()
    lintRules <- KDL.dashNodesAt "lint_rules" -< ()
    returnA -< RepoConfig{..}
  defaultsDecoder = proc () -> do
    fileGlobs <- KDL.optional $ KDL.argsAt "files" -< ()
    returnA -< (fileGlobs)
  getDefault def mFlags f = fromMaybe def $ mFlags >>= f

data HookConfig = HookConfig
  { name :: Text
  , cmdArgs :: NonEmpty Text
  , checkArgs :: [Text]
  , fixArgs :: [Text]
  , passFiles :: PassFilesMode
  , fileGlobs :: [Glob]
  }
  deriving (Show, Eq)

instance KDL.DecodeNode HookConfig where
  nodeDecoder = proc () -> do
    name <- KDL.arg -< ()
    finalize <- KDL.children $ KDL.nodeWith "command" $ commandDecoder -< ()
    fileGlobs <- KDL.children $ KDL.nodeWith "files" $ KDL.some KDL.arg -< ()
    returnA -< finalize name fileGlobs
   where
    commandDecoder = proc () -> do
      cmdArgs <- NonEmpty.fromList <$> KDL.some KDL.arg -< ()
      checkArgs <- KDL.children $ KDL.argsAt "check_args" -< ()
      fixArgs <- KDL.children $ KDL.argsAt "fix_args" -< ()
      passFiles <- KDL.children $ KDL.option PassFiles_XArgs $ KDL.argAt "pass_files" -< ()
      returnA -< (\name fileGlobs -> HookConfig{..})

{----- GlobalConfig -----}

data GlobalConfig = GlobalConfig
  { mode :: RunMode
  , format :: OutputFormat
  , useAbsolute :: Bool
  , maxOutputLines :: Int
  , maxParallelHooks :: Int
  }
  deriving (Show, Eq)

loadGlobalConfig :: IO GlobalConfig
loadGlobalConfig = do
  useGlobal <- not . maybe False (== "1") <$> lookupEnv "HOOKY_NO_GLOBAL"

  hookyConfigDir <- getXdgDirectory XdgConfig [osp|hooky|]
  let path = hookyConfigDir </> [osp|settings.kdl|]
  exists <- doesFileExist path

  content <- if useGlobal && exists then Text.decodeUtf8 <$> OsPath.readFile' path else pure ""
  case parseGlobalConfig content of
    Right config -> pure config
    Left e -> abort $ "Could not parse config: " <> OsPath.toText path <> "\n" <> e

parseGlobalConfig :: Text -> Either Text GlobalConfig
parseGlobalConfig = Bifunctor.first KDL.renderDecodeError . KDL.decodeWith decoder
 where
  decoder = KDL.document $ proc () -> do
    mFlags <- KDL.optional . KDL.nodeWith "flags" $ KDL.children flagsDecoder -< ()
    let mode = getFlag Mode_Check mFlags $ \(x, _, _) -> x
    let format = getFlag Format_Minimal mFlags $ \(_, x, _) -> x
    let useAbsolute = getFlag False mFlags $ \(_, _, x) -> x
    maxOutputLines <- KDL.option 5 $ KDL.argAt "max_output_lines" -< ()
    maxParallelHooks <- KDL.option 5 $ KDL.argAt "max_parallel_hooks" -< ()
    returnA -< GlobalConfig{..}
  flagsDecoder = proc () -> do
    mode <- KDL.optional $ KDL.argAt "--mode" -< ()
    format <- KDL.optional $ KDL.argAt "--format" -< ()
    useAbsolute <- KDL.optional . KDL.nodeWith "--absolute" $ pure True -< ()
    returnA -< (mode, format, useAbsolute)
  getFlag def mFlags f = fromMaybe def $ mFlags >>= f

{----- RunMode -----}

data RunMode = Mode_Check | Mode_Fix | Mode_FixAdd
  deriving (Show, Eq, Enum, Bounded)

allRunModes :: [RunMode]
allRunModes = [minBound .. maxBound]

parseRunMode :: Text -> Maybe RunMode
parseRunMode = flip Map.lookup x
 where
  x = Map.fromList [(renderRunMode m, m) | m <- allRunModes]

renderRunMode :: RunMode -> Text
renderRunMode = \case
  Mode_Check -> "check"
  Mode_Fix -> "fix"
  Mode_FixAdd -> "fix-add"

instance KDL.DecodeValue RunMode where
  valueDecoder = KDL.withDecoder KDL.valueDecoder $ \s ->
    case parseRunMode s of
      Nothing -> KDL.failM $ "Invalid --mode: " <> s
      Just mode -> pure mode

{----- OutputFormat -----}

instance KDL.DecodeValue OutputFormat where
  valueDecoder = KDL.withDecoder KDL.valueDecoder $ \s ->
    case parseOutputFormat s of
      Nothing -> KDL.failM $ "Invalid --format: " <> s
      Just format -> pure format

{----- LintRule -----}

data LintRule = LintRule
  { rule :: LintRuleRule
  , fileGlobs :: [Glob]
  }
  deriving (Show, Eq)

data LintRuleRule
  = LintRule_CheckBrokenSymlinks
  | LintRule_CheckCaseConflict
  | LintRule_CheckMergeConflict
  | LintRule_EndOfFileFixer
  | LintRule_NoCommitToBranch [Glob]
  | LintRule_TrailingWhitespace
  deriving (Show, Eq)

instance HasField "name" LintRule Text where
  getField LintRule{rule} = rule.name
instance HasField "name" LintRuleRule Text where
  getField = \case
    -- Must match lintRuleDecoder
    LintRule_CheckBrokenSymlinks{} -> "check_broken_symlinks"
    LintRule_CheckCaseConflict{} -> "check_case_conflict"
    LintRule_CheckMergeConflict{} -> "check_merge_conflict"
    LintRule_EndOfFileFixer{} -> "end_of_file_fixer"
    LintRule_NoCommitToBranch{} -> "no_commit_to_branch"
    LintRule_TrailingWhitespace{} -> "trailing_whitespace"

instance KDL.DecodeNode LintRule where
  nodeDecoder = proc () -> do
    name <- KDL.arg -< ()
    rule <- ruleDecoder -< name
    fileGlobs <- filesDecoder -< rule
    returnA -< LintRule{..}
   where
    ruleDecoder = proc name -> do
      case name of
        "check_broken_symlinks" ->
          returnA -< LintRule_CheckBrokenSymlinks
        "check_case_conflict" ->
          returnA -< LintRule_CheckCaseConflict
        "check_merge_conflict" ->
          returnA -< LintRule_CheckMergeConflict
        "end_of_file_fixer" ->
          returnA -< LintRule_EndOfFileFixer
        "no_commit_to_branch" -> do
          branchesRaw <- KDL.children $ KDL.dashChildrenAt "branches" -< ()
          let branches = if null branchesRaw then map toGlob ["main", "master"] else branchesRaw
          returnA -< LintRule_NoCommitToBranch branches
        "trailing_whitespace" ->
          returnA -< LintRule_TrailingWhitespace
        _ ->
          KDL.fail -< "Unknown lint rule: " <> name

    -- TODO: This should be in sync with whether the rule is LintActionNoFile
    filesDecoder = proc rule -> do
      case rule of
        LintRule_NoCommitToBranch{} -> do
          x <- KDL.optional $ KDL.children $ KDL.node @KDL.Node "files" -< ()
          case x of
            Just _ -> KDL.fail -< "'files' config is not supported for " <> rule.name
            Nothing -> returnA -< []
        LintRule_CheckBrokenSymlinks{}
        LintRule_CheckCaseConflict{}
        LintRule_CheckMergeConflict{}
        LintRule_EndOfFileFixer{}
        LintRule_TrailingWhitespace{} ->
            KDL.children $ KDL.argsAt "files" -< ()

{----- PassFilesMode -----}

data PassFilesMode
  = PassFiles_None
  | PassFiles_XArgs
  | PassFiles_XArgsParallel
  | PassFiles_File
  deriving (Show, Eq)

instance KDL.DecodeValue PassFilesMode where
  valueDecoder = KDL.withDecoder KDL.valueDecoder $ \case
    "xargs" -> pure PassFiles_XArgs
    "xargs_parallel" -> pure PassFiles_XArgsParallel
    "file" -> pure PassFiles_File
    "none" -> pure PassFiles_None
    s -> KDL.failM $ "Invalid pass_files value: " <> s

{----- Glob -----}

instance KDL.DecodeValue Glob where
  validValueTypeAnns _ = ["glob"]
  valueDecoder = toGlob <$> KDL.string
