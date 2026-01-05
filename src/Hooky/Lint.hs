{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Lint (
  LintRunConfig (..),
  LintReport (..),
  LintResult (..),
  runLintRules,
  renderLintReport,
  lintReportSuccess,

  -- * Re-exports from "Hooky.Config"
  LintRule (..),
  LintRuleRule (..),
  toGlob,
) where

import Control.Monad (forM, when)
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Monoid qualified as Monoid
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hooky.Config (
  Glob,
  LintRule (..),
  LintRuleRule (..),
  lintRuleName,
  matchesGlob,
  toGlob,
 )
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode)
import UnliftIO.Exception (tryJust)

type Repo = FilePath
type AutoFix = Bool

data LintRunConfig = LintRunConfig
  { repo :: Repo
  , autofix :: AutoFix
  , rules :: [LintRule]
  }

{----- runLintRules -----}

runLintRules :: LintRunConfig -> [FilePath] -> IO LintReport
runLintRules config files = do
  let allLinters = map (\rule -> (rule, fromLintRule rule)) config.rules
  nonFileLintResults <- runNonFileLintRules config allLinters
  allFilesLintResults <- runAllFilesLintRules config allLinters
  fileLintResults <- mapM (runPerFileLintRules config allLinters) files
  pure . LintReport . Map.unionsWith (<>) $
    [ Map.singleton Nothing nonFileLintResults
    , allFilesLintResults
    , Map.fromList fileLintResults
    ]

runNonFileLintRules ::
  LintRunConfig ->
  [(LintRule, LintAction)] ->
  IO [(Text, LintResult)]
runNonFileLintRules config allLinters =
  forM linters $ \(rule, run) -> do
    result <- run config
    pure (lintRuleName rule, result)
 where
  linters = [(rule, run) | (rule, LintActionNoFile run) <- allLinters]

runAllFilesLintRules ::
  LintRunConfig ->
  [(LintRule, LintAction)] ->
  IO (Map (Maybe FilePath) [(Text, LintResult)])
runAllFilesLintRules config allLinters = do
  (code, std_out, std_err) <- readProcessWithExitCode "git" ["-C", config.repo, "ls-files", "-z"] ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure n -> do
      -- TODO: better exceptions
      hPutStrLn stderr std_err
      error $ "hooky: git ls-files failed with code " <> show n

  let files =
        Set.fromList $
          map Text.unpack . Text.splitOn "\0" . Text.dropWhileEnd (== '\0') . Text.pack $
            std_out

  fmap (Map.fromListWith (<>) . concat) . forM linters $ \(rule, run) -> do
    results <- run config files
    pure [(Just fp, [(lintRuleName rule, result)]) | (fp, result) <- results]
 where
  linters = [(rule, run) | (rule, LintActionAllFiles run) <- allLinters]

runPerFileLintRules ::
  LintRunConfig ->
  [(LintRule, LintAction)] ->
  FilePath ->
  IO (Maybe FilePath, [(Text, LintResult)])
runPerFileLintRules config allLinters file =
  -- Skip reading file if there are no per-file linters to run
  (if null linters then pure Nothing else readFileMaybe file) >>= \case
    Nothing -> pure (Just file, [])
    Just contents1 -> do
      (results, contents2) <-
        mapAndFoldM
          ( \contents (rule, run) -> do
              -- TODO: check if file is valid for rule
              (result, contents') <- run config file contents
              let name = lintRuleName rule
              pure $
                if config.autofix
                  then ((name, result), contents')
                  -- TODO: show diff of fix failure
                  else ((name, if result == LintFixed then LintFailed "file would be changed" else result), contents)
          )
          contents1
          linters
      when (any ((== LintFixed) . snd) results) $ do
        Text.writeFile file contents2
      pure (Just file, results)
 where
  linters = [(rule, run) | (rule, LintActionPerFile run) <- allLinters]
  readFileMaybe fp =
    fmap (either (const Nothing) Just) $
      tryJust
        (\e -> if isDoesNotExistError e then Just e else Nothing)
        (Text.readFile fp)

  -- mapM that also threads state through the loop
  mapAndFoldM :: (Monad m) => (s -> a -> m (b, s)) -> s -> [a] -> m ([b], s)
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

{----- LintReport -----}

-- | Map from filepath to the hooks and their results.
newtype LintReport = LintReport {unwrap :: Map (Maybe FilePath) [(Text, LintResult)]}

lintReportSuccess :: LintReport -> Bool
lintReportSuccess = all ((== LintSuccess) . snd) . concat . Map.elems . (.unwrap)

renderLintReport :: LintReport -> Text
renderLintReport report = Text.intercalate "\n\n" $ failureMsgs ++ successMsgs
 where
  failureMsgs =
    [ Text.intercalate "\n" $
        (maybe "FAILURES" Text.pack mFile <> ":")
          : [ "- [" <> hook <> "] " <> msg
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

{----- LintAction -----}

data LintAction
  = LintActionNoFile (LintRunConfig -> IO LintResult)
  | LintActionAllFiles (LintRunConfig -> Set FilePath -> IO [(FilePath, LintResult)])
  | LintActionPerFile (LintRunConfig -> FilePath -> Text -> IO (LintResult, Text))

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

{----- Lint rule implementations -----}

lint_CheckBrokenSymlinks :: LintAction
lint_CheckBrokenSymlinks = LintActionAllFiles $ \_ files -> do
  map (,failure) <$> filterM (isBrokenSymlink files) (Set.toList files)
 where
  failure = LintFailed "File is a broken symlink. Remove or exclude from rule"
  filterM f xs =
    fmap catMaybes . forM xs $ \x -> do
      p <- f x
      pure $ if p then Just x else Nothing
  isBrokenSymlink files fp = do
    isLink <- Dir.pathIsSymbolicLink fp
    if not isLink
      then pure False
      else do
        linkTarget <- Dir.getSymbolicLinkTarget fp
        pure $ linkTarget `Set.notMember` files

lint_CheckCaseConflict :: LintAction
lint_CheckCaseConflict = LintActionAllFiles $ \_ files -> do
  let filePairs = allPairs . map Text.pack . Set.toList $ files
  pure $
    [ (Text.unpack fp, LintFailed $ "File conflicts with: " <> other)
    | (a, b) <- filter (\(a, b) -> Text.toLower a == Text.toLower b) filePairs
    , (fp, other) <- bothWays a b
    ]
 where
  allPairs = \case
    [] -> []
    x : xs -> [(x, y) | y <- xs] <> allPairs xs
  bothWays a b = [(a, b), (b, a)]

lint_CheckMergeConflict :: LintAction
lint_CheckMergeConflict = LintActionPerFile $ \_ _ contents -> do
  let result =
        case catMaybes $ zipWith getMergeConflict (Text.lines contents) [1 :: Int ..] of
          [] -> LintSuccess
          (lineNum, conflictPat) : _ ->
            LintFailed $
              "Merge conflict string found at line " <> (Text.pack . show) lineNum <> ": " <> (Text.pack . show) conflictPat
  pure (result, contents)
 where
  getMergeConflict line lineNum
    | pat : _ <- filter (`Text.isPrefixOf` line) prefixMatches = Just (lineNum, pat)
    | pat : _ <- filter (== line) fullMatches = Just (lineNum, pat)
    | otherwise = Nothing
  prefixMatches =
    [ "<<<<<<< "
    , "======= "
    , ">>>>>>> "
    ]
  fullMatches = ["======="]

lint_EndOfFileFixer :: LintAction
lint_EndOfFileFixer = LintActionPerFile $ \_ _ contents -> do
  case spanEnd (== '\n') contents of
    (_, suf) | Text.compareLength suf 1 == EQ -> pure (LintSuccess, contents)
    (stripped, _) -> pure (LintFixed, stripped <> "\n")
 where
  spanEnd p s =
    let suf = Text.takeWhileEnd p s
     in (Text.dropEnd (Text.length suf) s, suf)

lint_NoCommitToBranch :: [Glob] -> LintAction
lint_NoCommitToBranch branches = LintActionNoFile $ \config ->
  readProcessWithExitCode "git" ["-C", config.repo, "branch", "--show-current"] "" >>= \case
    (ExitFailure _, _, _) -> pure $ LintFailed "hooky: could not get current branch"
    (ExitSuccess, stdout, _) -> do
      let branch = (Text.strip . Text.pack) stdout
      pure $
        if any (`matchesGlob` branch) branches
          then LintFailed $ "cannot commit to branch: " <> branch
          else LintSuccess

lint_TrailingWhitespace :: LintAction
lint_TrailingWhitespace = LintActionPerFile $ \_ _ contents -> do
  -- FIXME
  pure (LintSuccess, contents)
