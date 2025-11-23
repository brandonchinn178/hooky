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
import Control.Monad (when, (<=<), (>=>))
import Data.Bifunctor qualified as Bifunctor
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

parseConfig :: Text -> Either Text Config
parseConfig = v2
  where
    v1 = KDL.decodeWith decodeConfig
    v2 = Bifunctor.first KDL.renderDecodeError . KDL.decodeWith_v2 configDecoder

decodeConfig :: KDL.Document -> KDL.DecodeResult Config
decodeConfig doc = do
  files <- KDL.decodeArgsAt "files" doc
  hooks <- fmap Map.fromList $ mapM decodeHook $ KDL.findNodes "hook" doc
  lintRules <- mapM decodeLintRule $ KDL.getDashChildren "lint_rules" doc
  pure Config{..}

configDecoder :: KDL.DocumentDecoder_v2 Config
configDecoder = KDL.document_v2 $ proc () -> do
  files <- KDL.many_v2 $ KDL.argAt_v2 "files" -< ()
  hooks <- fmap Map.fromList $ KDL.many_v2 $ KDL.node_v2 "hook" hookDecoder -< ()
  lintRules <- KDL.dashChildren_v2 "lint_rules" lintRuleDecoder -< ()
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

decodeHook :: KDL.AnnNode -> KDL.DecodeResult (Text, HookConfig)
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
  when (null files) $ KDL.fail $ "Hook '" <> name <> "' does not specify 'files', a required field"

  pure (name, HookConfig{..})

hookDecoder :: KDL.NodeDecoder_v2 (Text, HookConfig)
hookDecoder = proc () -> do
  name <- KDL.required_v2 KDL.arg_v2 -< ()
  (cmdArgs, checkArgs, fixArgs, passFiles) <- KDL.children_v2 $ KDL.required_v2 $ KDL.node_v2 "command" commandDecoder -< ()
  files <- KDL.children_v2 $ KDL.some_v2 $ KDL.argAt_v2 "files" -< ()
  returnA -< (name, HookConfig{..})
  where
    commandDecoder = proc () -> do
      cmdArgs <- KDL.some_v2 $ KDL.arg_v2 -< ()
      checkArgs <- KDL.children_v2 $ KDL.many_v2 $ KDL.argAt_v2 "check_arg" -< ()
      fixArgs <- KDL.children_v2 $ KDL.many_v2 $ KDL.argAt_v2 "fix_arg" -< ()
      passFiles <- KDL.children_v2 $ KDL.setDefault_v2 PassFiles_XArgs $ KDL.optional_v2 $ KDL.argAt_v2 "pass_files" -< ()
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
    -- Must match decodeLintRule
    LintRule_CheckBrokenSymlinks{} -> "check_broken_symlinks"
    LintRule_CheckCaseConflict{} -> "check_case_conflict"
    LintRule_CheckMergeConflict{} -> "check_merge_conflict"
    LintRule_EndOfFileFixer{} -> "end_of_file_fixer"
    LintRule_NoCommitToBranch{} -> "no_commit_to_branch"
    LintRule_TrailingWhitespace{} -> "trailing_whitespace"

decodeLintRule :: KDL.AnnNode -> KDL.DecodeResult LintRule
decodeLintRule node = do
  name <- decodeReqArg "Encountered lint rule without a name" node

  let lintNodes = KDL.nodeChildren node
  rule <-
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
          case KDL.getDashChildren "branches" lintNodes of
            [] -> pure [toGlob "main", toGlob "master"]
            branches -> mapM (decodeReqArg "no_commit_to_branch.branch missing name") branches
        pure $ LintRule_NoCommitToBranch branches
      "trailing_whitespace" ->
        pure LintRule_TrailingWhitespace
      _ ->
        KDL.fail $ "Unknown lint rule: " <> name

  files <- KDL.decodeArgsAt "files" lintNodes

  pure LintRule{..}

lintRuleDecoder :: KDL.NodeDecoder_v2 LintRule
lintRuleDecoder = proc () -> do
  rule <- ruleDecoder -< ()
  files <- KDL.children_v2 $ KDL.many_v2 $ KDL.argAt_v2 "files" -< ()
  returnA -< LintRule{..}
  where
    ruleDecoder = proc () -> do
      name <- KDL.required_v2 $ KDL.arg_v2 -< ()
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
            KDL.children_v2 $
              KDL.setDefault_v2 (map toGlob ["main", "master"]) . fmap maybeList $
                KDL.dashChildren_v2 "branches" $
                  KDL.required_v2 KDL.arg_v2
            -< ()
          returnA -< LintRule_NoCommitToBranch branches
        "trailing_whitespace" ->
          returnA -< LintRule_TrailingWhitespace
        _ ->
          KDL.lift_v2 KDL.fail_v2 -< "Unknown lint rule: " <> name

    maybeList xs = if null xs then Nothing else Just xs

{----- PassFilesMode -----}

data PassFilesMode
  = PassFiles_None
  | PassFiles_XArgs
  | PassFiles_File
  deriving (Show, Eq)

instance KDL.DecodeValue PassFilesMode where
  decodeValue = KDL.decodeValue >=> \case
    Nothing -> pure PassFiles_None
    Just "xargs" -> pure PassFiles_XArgs
    Just "file" -> pure PassFiles_File
    Just s -> KDL.fail $ "Invalid pass_files value: " <> s

instance KDL.DecodeValue_v2 PassFilesMode where
  valueDecoder_v2 = KDL.withDecoder_v2 KDL.valueDecoder_v2 $ \case
    Nothing -> pure PassFiles_None
    Just "xargs" -> pure PassFiles_XArgs
    Just "file" -> pure PassFiles_File
    Just s -> KDL.fail_v2 $ "Invalid pass_files value: " <> s

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
  decodeValue = fmap toGlob . KDL.decodeValue

instance KDL.DecodeValue_v2 Glob where
  validTypeAnns_v2 _ = ["glob"]
  valueDecoder_v2 = toGlob <$> KDL.text_v2

{----- Utilities -----}

decodeReqArg :: KDL.DecodeValue a => Text -> KDL.AnnNode -> KDL.DecodeResult a
decodeReqArg msg = maybe (KDL.fail msg) pure <=< KDL.decodeArg
