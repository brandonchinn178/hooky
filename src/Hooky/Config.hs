{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hooky.Config (
  Config (..),
  parseConfig,
) where

import Control.Monad ((<=<), (>=>))
import Data.KDL qualified as KDL
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

data Config = Config
  { files :: [Glob]
  , hooks :: Map Text HookConfig
  , lintRules :: [LintRule]
  }
  deriving (Show, Eq)

data HookConfig = HookConfig
  { name :: Text
  , cmdArgs :: [Text]
  , checkArgs :: [Text]
  , fixArgs :: [Text]
  , passFiles :: PassFilesMode
  , files :: [Glob]
  }
  deriving (Show, Eq)

data PassFilesMode
  = PassFiles_None
  | PassFiles_XArgs
  | PassFiles_File
  deriving (Show, Eq)

data LintRule
  = LintRule_CheckBrokenSymlinks
  | LintRule_CheckCaseConflict
  | LintRule_CheckMergeConflict
  | LintRule_EndOfFileFixer
  | LintRule_NoCommitToBranch [Glob]
  | LintRule_TrailingWhitespace
  deriving (Show, Eq)

parseConfig :: Text -> Either Text Config
parseConfig = KDL.decodeWith decodeConfig

decodeConfig :: KDL.Document -> KDL.Decoder Config
decodeConfig doc = do
  files <- KDL.decodeArgsAt "files" doc
  hooks <- fmap Map.fromList $ mapM decodeHook $ KDL.findNodes "hook" doc
  lintRules <- mapM decodeLintRule $ KDL.getDashChildren "lint_rules" doc
  pure Config{..}

decodeHook :: KDL.AnnNode -> KDL.Decoder (Text, HookConfig)
decodeHook hookNode = do
  let hookChildren = KDL.nodeChildren hookNode

  name <- decodeReqArg "Encountered hook without a name" hookNode

  commandNode <- maybe (KDL.fail $ "Hook '" <> name <> "' is missing command") pure $ KDL.findNode "command" hookChildren
  let commandChildren = KDL.nodeChildren commandNode

  cmdArgs <- KDL.decodeArgs commandNode
  checkArgs <- KDL.decodeArgsAt "check_arg" commandChildren
  fixArgs <- KDL.decodeArgsAt "fix_arg" commandChildren
  passFiles <- fromMaybe PassFiles_XArgs <$> KDL.decodeArgAt "pass_files" commandChildren

  files <- KDL.decodeArgsAt "files" hookChildren

  pure (name, HookConfig{..})

decodeLintRule :: KDL.AnnNode -> KDL.Decoder LintRule
decodeLintRule node = do
  name <- decodeReqArg "Encountered lint rule without a name" node
  case name of
    "check_broken_symlinks" ->
      pure LintRule_CheckBrokenSymlinks
    "check_case_conflict" ->
      pure LintRule_CheckCaseConflict
    "check_merge_conflict" ->
      pure LintRule_CheckMergeConflict
    "end_of_file_fixer" ->
      pure LintRule_EndOfFileFixer
    "no_commit_to_branch" -> do
      branches <-
        case KDL.getDashChildren "branches" $ KDL.nodeChildren node of
          [] -> pure [toGlob "main", toGlob "master"]
          branches -> mapM (decodeReqArg "no_commit_to_branch.branch missing name") branches
      pure $ LintRule_NoCommitToBranch branches
    "trailing_whitespace" ->
      pure LintRule_TrailingWhitespace
    _ ->
      KDL.fail $ "Unknown lint rule: " <> name

decodeReqArg :: KDL.DecodeValue a => Text -> KDL.AnnNode -> KDL.Decoder a
decodeReqArg msg = maybe (KDL.fail msg) pure <=< KDL.decodeArg

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

instance KDL.DecodeValue PassFilesMode where
  decodeValue = KDL.decodeValue >=> \case
    Nothing -> pure PassFiles_None
    Just "xargs" -> pure PassFiles_XArgs
    Just "file" -> pure PassFiles_File
    Just s -> KDL.fail $ "Invalid pass_files value: " <> s

instance KDL.DecodeValue Glob where
  decodeValue = fmap toGlob . KDL.decodeValue
