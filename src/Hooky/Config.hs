{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Config (
  -- * Config
  Config (..),
  loadConfig,
  RepoConfig (..),
  HookConfig (..),
  PassFilesMode (..),
  LintRule (..),
  LintRuleRule (..),

  -- * Glob
  Glob (..),
  matchesGlob,
  matchesGlobs,
  toGlob,
  renderGlob,
) where

import Control.Arrow (returnA)
import Control.Monad (unless)
import Data.Bifunctor qualified as Bifunctor
import Data.List (partition, tails)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import Hooky.Error (abort)
import KDL.Arrow qualified as KDL
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

data Config = Config
  { repoConfigPath :: FilePath
  , repo :: RepoConfig
  , skippedHooks :: Set Text
  }
  deriving (Show, Eq)

loadConfig :: FilePath -> IO Config
loadConfig repoConfigPath = do
  configFileExists <- doesFileExist repoConfigPath
  unless configFileExists $ do
    abort $ "Config file doesn't exist: " <> Text.pack repoConfigPath
  repo <- either abort pure . parseRepoConfig =<< Text.readFile repoConfigPath

  let fromCSV = Text.splitOn "," . Text.pack
  skippedHooks <- Set.fromList . maybe [] fromCSV <$> lookupEnv "SKIP"

  pure Config{..}

data RepoConfig = RepoConfig
  { files :: [Glob]
  , hooks :: [HookConfig]
  , lintRules :: [LintRule]
  }
  deriving (Show, Eq)

parseRepoConfig :: Text -> Either Text RepoConfig
parseRepoConfig = Bifunctor.first KDL.renderDecodeError . KDL.decodeWith decoder
 where
  decoder = KDL.document $ proc () -> do
    files <- KDL.argsAt "files" -< ()
    hooks <- KDL.many $ KDL.node "hook" -< ()
    lintRules <- KDL.dashNodesAt "lint_rules" -< ()
    returnA -< RepoConfig{..}

data HookConfig = HookConfig
  { name :: Text
  , cmdArgs :: NonEmpty Text
  , checkArgs :: [Text]
  , fixArgs :: [Text]
  , passFiles :: PassFilesMode
  , files :: [Glob]
  }
  deriving (Show, Eq)

instance KDL.DecodeNode HookConfig where
  nodeDecoder = proc () -> do
    name <- KDL.arg -< ()
    finalize <- KDL.children $ KDL.nodeWith "command" $ commandDecoder -< ()
    files <- KDL.children $ KDL.nodeWith "files" $ KDL.some KDL.arg -< ()
    returnA -< finalize name files
   where
    commandDecoder = proc () -> do
      cmdArgs <- NonEmpty.fromList <$> KDL.some KDL.arg -< ()
      checkArgs <- KDL.children $ KDL.argsAt "check_args" -< ()
      fixArgs <- KDL.children $ KDL.argsAt "fix_args" -< ()
      passFiles <- KDL.children $ KDL.option PassFiles_XArgs $ KDL.argAt "pass_files" -< ()
      returnA -< (\name files -> HookConfig{..})

{----- LintRule -----}

data LintRule = LintRule
  { rule :: LintRuleRule
  , files :: [Glob]
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
  getField LintRule{rule} =
    case rule of
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
    files <- KDL.children $ KDL.argsAt "files" -< ()
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

{----- PassFilesMode -----}

data PassFilesMode
  = PassFiles_None
  | PassFiles_XArgs
  | PassFiles_XArgsParallel
  | PassFiles_File
  deriving (Show, Eq)

instance KDL.DecodeValue PassFilesMode where
  valueDecoder = KDL.withDecoder KDL.valueDecoder $ \case
    Nothing -> pure PassFiles_None
    Just "xargs" -> pure PassFiles_XArgs
    Just "xargs_parallel" -> pure PassFiles_XArgsParallel
    Just "file" -> pure PassFiles_File
    Just s -> KDL.failM $ "Invalid pass_files value: " <> s

{----- Glob -----}

-- | TODO: make proper data type
-- (isNegate, [Left isStarStar, Right lit])
newtype Glob = Glob (Bool, [Either Bool String])
  deriving (Eq)

instance Show Glob where
  showsPrec _ glob = showString "toGlob \"" . showString (Text.unpack $ renderGlob glob) . showString "\""

toGlob :: Text -> Glob
toGlob = Glob . parse0 . Text.unpack
 where
  parse0 = \case
    '!' : cs -> (True, parse1 cs)
    cs -> (False, parse1 cs)

  parse1 = \case
    '/' : cs -> parse2 cs
    cs -> Left True : dropLeading (Left True) (parse2 cs)

  parse2 = \case
    '*' : '*' : cs -> Left True : parse2 (dropLeading '/' cs)
    '*' : cs -> Left False : parse2 cs
    -- TODO: collapse all consecutive Rights
    c : cs -> Right [c] : parse2 cs
    [] -> []

  dropLeading x = \case
    a : as | a == x -> as
    as -> as

renderGlob :: Glob -> Text
renderGlob (Glob (isNegate, parts)) = (if isNegate then "!" else "") <> foldMap go parts
 where
  go = \case
    Left True -> "**/"
    Left False -> "*"
    Right s -> Text.pack s

matchesGlob :: Glob -> Text -> Bool
matchesGlob (Glob (isNegate, parts)) = (if isNegate then not else id) . go parts
 where
  go [] = Text.null
  go (Left True : rest) = any (go rest) . wildcardDirs
  go (Left False : rest) = any (go rest) . wildcardFile
  go (Right s : rest) = maybe False (go rest) . Text.stripPrefix (Text.pack s)

  wildcardDirs = map (Text.intercalate "/") . tails . Text.splitOn "/"
  wildcardFile fp =
    let (pre, post) = Text.breakOn "/" fp
     in map (<> post) $ Text.tails pre

matchesGlobs :: [Glob] -> Text -> Bool
matchesGlobs globs s =
  and
    [ null posGlobs || any matches posGlobs
    , null negGlobs || all matches negGlobs
    ]
 where
  matches = (`matchesGlob` s)
  (negGlobs, posGlobs) = partition (\(Glob (x, _)) -> x) globs

instance KDL.DecodeValue Glob where
  validValueTypeAnns _ = ["glob"]
  valueDecoder = toGlob <$> KDL.text
