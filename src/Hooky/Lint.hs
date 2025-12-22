{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Hooky.Lint (
  LintRunConfig (..),
  LintReport (..),
  LintResult (..),
  runLintRules,
  renderLintReport,
) where

import Control.Monad (forM, when)
import Data.Either (partitionEithers)
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid qualified as Monoid
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hooky.Config (
  Glob,
  LintRule (..),
  LintRuleRule (..),
  lintRuleName,
 )
import System.Directory qualified as Dir

type Repo = FilePath
type AutoFix = Bool

data LintRunConfig = LintRunConfig
  { repo :: Repo
  , autofix :: AutoFix
  , rules :: [LintRule]
  }

runLintRules :: LintRunConfig -> [FilePath] -> IO LintReport
runLintRules config files = do
  let linters = map (\rule -> (rule, fromLintRule rule)) config.rules
  let (nonFileLinters, fileLinters) =
        partitionEithers
          [ case action of
              LintActionNoFile run -> Left (rule, run)
              LintActionPerFile run -> Right (rule, run)
          | (rule, action) <- linters
          ]

  nonFileLinterResults <-
    fmap (Nothing,) $
      forM nonFileLinters $ \(rule, run) -> do
        result <- run config
        pure (lintRuleName rule, result)

  fileLinterResults <-
    forM files $ \file -> do
      contents1 <- Text.readFile file
      (results, contents2) <-
        mapAndFoldM
          ( \contents (rule, run) -> do
              -- TODO: check if file is valid for rule
              (result, contents') <- run config file contents
              pure ((lintRuleName rule, result), if config.autofix then contents' else contents)
          )
          contents1
          fileLinters
      when (config.autofix && contents2 /= contents1) $ Text.writeFile file contents2
      pure (Just file, results)

  pure . LintReport . Map.fromList $ nonFileLinterResults : fileLinterResults
  where
    -- mapM that also threads state through the loop
    mapAndFoldM :: Monad m => (s -> a -> m (b, s)) -> s -> [a] -> m ([b], s)
    mapAndFoldM f s0 as = do
      (acc, s') <-
        foldlM
          ( \(acc, s) a -> do
              (b, s') <- f s a
              pure (b : acc, s')
          )
          ([], s0)
          as
      pure (reverse acc, s')

-- | Map from filepath to the hooks and their results.
newtype LintReport = LintReport {unwrap :: Map (Maybe FilePath) [(Text, LintResult)]}

renderLintReport :: LintReport -> Text
renderLintReport report = Text.intercalate "\n\n" $ failureMsgs ++ successMsgs
  where
    failureMsgs =
      [ Text.intercalate "\n" $
          (maybe "<no file>" Text.pack mFile <> ":") :
            [ "- [" <> hook <> "] " <> msg
            | (hook, result) <- results
            , Just msg <-
                pure $
                  case result of
                    LintSuccess -> Nothing
                    LintFixed -> Just "FIXED"
                    LintFailed msg -> Just msg
            ]
      | (mFile, results) <- Map.toAscList report.unwrap
      , any ((/= LintSuccess) . snd) results
      ]

    successfulHooks = getSuccessfulHooks report
    successMsgs =
      if null successfulHooks
        then []
        else [Text.intercalate "\n" $ "Hooks passed:" : map ("- " <>) successfulHooks]

getSuccessfulHooks :: LintReport -> [Text]
getSuccessfulHooks =
  -- Map (Maybe FilePath) [(Text, LintResult)]
  --   => [(Text, LintResult)]
  --   => [(Text, isSuccess)]
  --   => Map Text (All isSuccess)
  Map.keys
    . Map.filter Monoid.getAll
    . Map.fromListWith (<>)
    . map (fmap (Monoid.All . (== LintSuccess)))
    . (concat . Map.elems)
    . (.unwrap)

data LintAction
  = LintActionNoFile (LintRunConfig -> IO LintResult)
  | LintActionPerFile (LintRunConfig -> FilePath -> Text -> IO (LintResult, Text))

notFixable ::
  (LintRunConfig -> FilePath -> IO LintResult) ->
  (LintRunConfig -> FilePath -> Text -> IO (LintResult, Text))
notFixable f = \config file contents -> (,contents) <$> f config file

data LintResult = LintSuccess | LintFixed | LintFailed Text
  deriving (Show, Eq)

fromLintRule :: LintRule -> LintAction
fromLintRule LintRule{rule} =
  case rule of
    LintRule_CheckBrokenSymlinks -> lint_CheckBrokenSymlinks
    LintRule_CheckCaseConflict -> lint_CheckCaseConflict
    LintRule_CheckMergeConflict -> lint_CheckMergeConflict
    LintRule_EndOfFileFixer -> lint_EndOfFileFixer
    LintRule_NoCommitToBranch branches -> lint_NoCommitToBranch branches
    LintRule_TrailingWhitespace -> lint_TrailingWhitespace

lint_CheckBrokenSymlinks :: LintAction
lint_CheckBrokenSymlinks = LintActionPerFile . notFixable $ \_ file -> do
  isLink <- Dir.pathIsSymbolicLink file
  linkTarget <- if isLink then Just <$> Dir.getSymbolicLinkTarget file else pure Nothing
  linkExists <- traverse Dir.doesPathExist linkTarget
  pure $
    if linkExists == Just False
      then LintFailed "File is a broken symlink. Remove or exclude from rule"
      else LintSuccess

-- FIXME
lint_CheckCaseConflict :: LintAction
lint_CheckCaseConflict = LintActionPerFile . notFixable $ \_ _ -> pure LintSuccess

-- FIXME
lint_CheckMergeConflict :: LintAction
lint_CheckMergeConflict = LintActionPerFile . notFixable $ \_ _ -> pure LintSuccess

-- FIXME
lint_EndOfFileFixer :: LintAction
lint_EndOfFileFixer = LintActionPerFile $ \_ _ contents -> pure (LintSuccess, contents)

-- FIXME
lint_NoCommitToBranch :: [Glob] -> LintAction
lint_NoCommitToBranch _ = LintActionNoFile $ \_ -> pure LintSuccess

-- FIXME
lint_TrailingWhitespace :: LintAction
lint_TrailingWhitespace = LintActionPerFile $ \_ _ contents -> pure (LintSuccess, contents)
