{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Config (
  Config (..),
  parseConfig,
  Glob (..),
  HookConfig (..),
  PassFilesMode (..),
  LintRule (..),
  LintRuleRule (..),
  lintRuleName,
) where

import Control.Arrow (returnA)
import Data.Bifunctor qualified as Bifunctor
import Data.KDL.Decoder.Arrow qualified as KDL
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import qualified Data.KDL.Decoder.DecodeM as KDL

data Config = Config
  { files :: [Glob]
  , hooks :: Map Text HookConfig
  , lintRules :: [LintRule]
  }
  deriving (Show, Eq)

parseConfig :: Text -> Either Text Config
parseConfig = Bifunctor.first KDL.renderDecodeError . KDL.decodeWith decoder
 where
  decoder = KDL.document $ proc () -> do
    files <- KDL.argsAt "files" -< ()
    hooks <- fmap toHookMap $ KDL.many $ KDL.node "hook" -< ()
    lintRules <- KDL.dashNodesAt "lint_rules" -< ()
    returnA -< Config{..}

  toHookMap = Map.fromList . map (\config -> (config.name, config))

data HookConfig = HookConfig
  { name :: Text
  , cmdArgs :: [Text]
  , checkArgs :: [Text]
  , fixArgs :: [Text]
  , passFiles :: PassFilesMode
  , files :: [Glob]
  }
  deriving (Show, Eq)

instance KDL.DecodeBaseNode HookConfig where
  baseNodeDecoder = proc () -> do
    name <- KDL.arg -< ()
    finalize <- KDL.children $ KDL.nodeWith "command" [] $ commandDecoder -< ()
    files <- KDL.children $ KDL.nodeWith "files" [] $ KDL.some KDL.arg -< ()
    returnA -< finalize name files
   where
    commandDecoder = proc () -> do
      cmdArgs <- KDL.some KDL.arg -< ()
      checkArgs <- KDL.children $ KDL.argsAt "check_arg" -< ()
      fixArgs <- KDL.children $ KDL.argsAt "fix_arg" -< ()
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

lintRuleName :: LintRule -> Text
lintRuleName LintRule{rule} =
  case rule of
    -- Must match lintRuleDecoder
    LintRule_CheckBrokenSymlinks{} -> "check_broken_symlinks"
    LintRule_CheckCaseConflict{} -> "check_case_conflict"
    LintRule_CheckMergeConflict{} -> "check_merge_conflict"
    LintRule_EndOfFileFixer{} -> "end_of_file_fixer"
    LintRule_NoCommitToBranch{} -> "no_commit_to_branch"
    LintRule_TrailingWhitespace{} -> "trailing_whitespace"

instance KDL.DecodeBaseNode LintRule where
  baseNodeDecoder = proc () -> do
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
  | PassFiles_File
  deriving (Show, Eq)

instance KDL.DecodeBaseValue PassFilesMode where
  baseValueDecoder = KDL.withDecoder KDL.baseValueDecoder $ \case
    Nothing -> pure PassFiles_None
    Just "xargs" -> pure PassFiles_XArgs
    Just "file" -> pure PassFiles_File
    Just s -> KDL.failM $ "Invalid pass_files value: " <> s

{----- Glob -----}

{- | TODO: make proper data type
(isNegate, [Left isStarStar, Right lit])
-}
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
    '/' : cs -> Left True : parse2 cs
    cs -> parse2 cs

  parse2 = \case
    '*' : '*' : cs -> Left True : parse2 cs
    '*' : cs -> Left False : parse2 cs
    -- TODO: collapse all consecutive Rights
    c : cs -> Right [c] : parse2 cs
    [] -> []

renderGlob :: Glob -> Text
renderGlob (Glob (isNegate, parts)) = (if isNegate then "!" else "") <> foldMap go parts
 where
  go = \case
    Left True -> "**"
    Left False -> "*"
    Right s -> Text.pack s

instance KDL.DecodeBaseValue Glob where
  baseValueTypeAnns _ = ["glob"]
  baseValueDecoder = toGlob <$> KDL.text
