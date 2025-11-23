{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.KDL qualified as KDL
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

data Config = Config
  { files :: [Glob]
  , hooks :: Map Text HookConfig
  , lintRules :: [LintRule]
  }
  deriving (Show, Eq)

parseConfig :: Text -> Either Text Config
parseConfig = Bifunctor.first KDL.renderDecodeError . KDL.decodeWith configDecoder

configDecoder :: KDL.DocumentDecoder Config
configDecoder = KDL.document $ proc () -> do
  files <- KDL.many $ KDL.argAt "files" -< ()
  hooks <- fmap Map.fromList $ KDL.many $ KDL.node "hook" hookDecoder -< ()
  lintRules <- KDL.dashChildren "lint_rules" lintRuleDecoder -< ()
  returnA -< Config{..}

data HookConfig = HookConfig
  { name :: Text
  , cmdArgs :: [Text]
  , checkArgs :: [Text]
  , fixArgs :: [Text]
  , passFiles :: PassFilesMode
  , files :: [Glob]
  }
  deriving (Show, Eq)

hookDecoder :: KDL.NodeDecoder (Text, HookConfig)
hookDecoder = proc () -> do
  name <- KDL.required KDL.arg -< ()
  (cmdArgs, checkArgs, fixArgs, passFiles) <- KDL.children $ KDL.required $ KDL.node "command" commandDecoder -< ()
  files <- KDL.children $ KDL.some $ KDL.argAt "files" -< ()
  returnA -< (name, HookConfig{..})
  where
    commandDecoder = proc () -> do
      cmdArgs <- KDL.some $ KDL.arg -< ()
      checkArgs <- KDL.children $ KDL.many $ KDL.argAt "check_arg" -< ()
      fixArgs <- KDL.children $ KDL.many $ KDL.argAt "fix_arg" -< ()
      passFiles <- KDL.children $ KDL.setDefault PassFiles_XArgs $ KDL.optional $ KDL.argAt "pass_files" -< ()
      returnA -< (cmdArgs, checkArgs, fixArgs, passFiles)

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

lintRuleDecoder :: KDL.NodeDecoder LintRule
lintRuleDecoder = proc () -> do
  rule <- ruleDecoder -< ()
  files <- KDL.children $ KDL.many $ KDL.argAt "files" -< ()
  returnA -< LintRule{..}
  where
    ruleDecoder = proc () -> do
      name <- KDL.required $ KDL.arg -< ()
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
          branches <-
            KDL.children $
              KDL.setDefault (map toGlob ["main", "master"]) . fmap maybeList $
                KDL.dashChildren "branches" $
                  KDL.required KDL.arg
            -< ()
          returnA -< LintRule_NoCommitToBranch branches
        "trailing_whitespace" ->
          returnA -< LintRule_TrailingWhitespace
        _ ->
          KDL.lift KDL.fail -< "Unknown lint rule: " <> name

    maybeList xs = if null xs then Nothing else Just xs

{----- PassFilesMode -----}

data PassFilesMode
  = PassFiles_None
  | PassFiles_XArgs
  | PassFiles_File
  deriving (Show, Eq)

instance KDL.DecodeValue PassFilesMode where
  valueDecoder = KDL.withDecoder KDL.valueDecoder $ \case
    Nothing -> pure PassFiles_None
    Just "xargs" -> pure PassFiles_XArgs
    Just "file" -> pure PassFiles_File
    Just s -> KDL.fail $ "Invalid pass_files value: " <> s

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

instance KDL.DecodeValue Glob where
  validTypeAnns _ = ["glob"]
  valueDecoder = toGlob <$> KDL.text
