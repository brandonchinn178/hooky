{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.LintSpec (spec) where

import Control.Monad (forM_, (<=<))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Hooky.Config (Config (..), RepoConfig (..))
import Hooky.Config qualified as LintRule (LintRule (..))
import Hooky.Internal.GitFile (GitFile (..))
import Hooky.Lint (
  LintOptions (..),
  LintRule (..),
  LintRuleRule (..),
  lintReportSuccess,
  renderLintReport,
  runLintRules,
  toGlob,
 )
import Hooky.TestUtils.Git (withGitRepo)
import Skeletest
import Skeletest.Predicate qualified as P
import System.Directory (
  createDirectory,
  createDirectoryIfMissing,
  createDirectoryLink,
  createFileLink,
  getCurrentDirectory,
  removeFile,
 )
import System.FilePath ((</>))
import System.Timeout (timeout)
import UnliftIO.Exception (SomeException)

spec :: Spec
spec = do
  describe "check_broken_symlinks" $ do
    let config = defaultConfig LintRule_CheckBrokenSymlinks

    it "succeeds when all symlinks are valid" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo.txt", "foo-link.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` True

    it "handles symlinks to directories" $ do
      report <-
        withGitRepo $ \git -> do
          createDirectory "foo"
          writeFile "foo/bar.txt" ""
          createDirectoryLink "foo" "foo-link"
          git.exec ["add", "foo", "foo-link"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` True

    it "handles relative path symlinks" $ do
      report <-
        withGitRepo $ \git -> do
          createDirectory "subdir"
          createDirectory "subdir/subdir2"
          -- subdir/top-level-link.txt -> ../top-level.txt
          writeFile "top-level.txt" ""
          createFileLink "../top-level.txt" "subdir/top-level-link.txt"
          -- subdir/nested-link.txt -> nested.txt
          writeFile "subdir/nested.txt" ""
          createFileLink "nested.txt" "subdir/nested-link.txt"
          git.exec ["add", "top-level.txt", "subdir"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` True

    it "handles symlink with .. in deep symlink" $ do
      report <-
        withGitRepo $ \git -> do
          createDirectoryIfMissing True "subdir1/subdir2/subdir3"
          writeFile "subdir1/subdir2/nested.txt" ""
          writeFile "subdir1/subdir2/subdir3/.gitkeep" ""
          createFileLink "subdir1/subdir2/subdir3" "deep-link"
          createFileLink "deep-link/../nested.txt" "nested-link.txt"
          git.exec ["add", "subdir1", "deep-link", "nested-link.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` True

    it "fails when a symlink is broken" $ do
      report <-
        withGitRepo $ \git -> do
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo-link.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when target is not tracked" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo-link.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` False

    it "fails when target is deleted" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo.txt", "foo-link.txt"]
          git.exec ["commit", "-m", "Initial commit"]
          git.exec ["rm", "foo.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` False

    it "fails when target is absolute path" $ do
      report <-
        withGitRepo $ \git -> do
          root <- getCurrentDirectory
          writeFile "foo.txt" ""
          createFileLink (root </> "foo.txt") "foo-link.txt"
          git.exec ["add", "foo.txt", "foo-link.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` False

    it "skips files failing glob" . withGitRepo $ \git -> do
      createFileLink "foo.txt" "foo-link.txt"
      git.exec ["add", "foo-link.txt"]
      report <-
        runLintRules
          git.client
          (withFiles ["!*.txt"] config)
          defaultOptions
      lintReportSuccess report `shouldBe` True

  describe "check_case_conflict" $ do
    let config = defaultConfig LintRule_CheckCaseConflict

    it "succeeds when no files conflict" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" ""
          writeFile "bar.txt" ""
          git.exec ["add", "foo.txt", "bar.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` True

    it "fails when files conflict" $ do
      report <-
        withGitRepo $ \git -> do
          git.exec ["config", "core.ignorecase", "false"]
          writeFile "foo.txt" ""
          git.exec ["add", "foo.txt"]
          removeFile "foo.txt"
          writeFile "FOO.TXT" ""
          git.exec ["add", "FOO.TXT"]
          git.exec ["checkout", "foo.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when new file conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          git.exec ["config", "core.ignorecase", "false"]
          writeFile "foo.txt" ""
          git.exec ["add", "foo.txt"]
          git.exec ["commit", "-m", "Initial commit"]
          removeFile "foo.txt"
          writeFile "FOO.TXT" ""
          git.exec ["add", "FOO.TXT"]
          git.exec ["checkout", "foo.txt"]
          runLintRules git.client config defaultOptions
      lintReportSuccess report `shouldBe` False

    it "handles large number of files" . withGitRepo $ \git -> do
      forM_ [1 .. 10000 :: Int] $ \x ->
        writeFile ("test-" <> show x) ""
      git.exec ["add", "."]
      maybe (failTest "Timed out") pure <=< timeout (100 * 1000) $ do
        report1 <- runLintRules git.client config defaultOptions
        lintReportSuccess report1 `shouldBe` True

    it "skips files failing glob" . withGitRepo $ \git -> do
      git.exec ["config", "core.ignorecase", "false"]
      writeFile "foo.txt" "" >> git.exec ["add", "foo.txt"]
      removeFile "foo.txt" >> writeFile "FOO.TXT" "" >> git.exec ["add", "FOO.TXT"] >> git.exec ["checkout", "foo.txt"]
      report <-
        runLintRules
          git.client
          (withFiles ["FOO.txt"] config)
          defaultOptions
      lintReportSuccess report `shouldBe` True

  describe "check_merge_conflict" $ do
    let config = defaultConfig LintRule_CheckMergeConflict

    it "succeeds when there are no merge conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "Initial commit"]
          git.exec ["switch", "-c", "branch1"] >> writeFile "foo.txt" "branch1" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch1"]
          git.exec ["switch", "-c", "branch2"] >> writeFile "bar.txt" "branch2" >> git.exec ["add", "bar.txt"] >> git.exec ["commit", "-m", "branch2"]
          git.exec ["switch", "main"]
          git.exec ["merge", "branch1", "branch2"]
          runLintRules git.client config defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` True

    it "fails when there are merge conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "Initial commit"]
          git.exec ["switch", "-c", "branch1", "main"] >> writeFile "foo.txt" "branch1" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch1"]
          git.exec ["switch", "-c", "branch2", "main"] >> writeFile "foo.txt" "branch2" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch2"]
          git.exec ["switch", "main"]
          git.exec ["merge", "branch1", "branch2"] `shouldSatisfy` P.throws (P.anything @SomeException)
          runLintRules git.client config defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "skips files failing glob" . withGitRepo $ \git -> do
      writeFile "foo.txt" "" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "Initial commit"]
      git.exec ["switch", "-c", "branch1", "main"] >> writeFile "foo.txt" "branch1" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch1"]
      git.exec ["switch", "-c", "branch2", "main"] >> writeFile "foo.txt" "branch2" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch2"]
      git.exec ["switch", "main"]
      git.exec ["merge", "branch1", "branch2"] `shouldSatisfy` P.throws (P.anything @SomeException)
      report <-
        runLintRules
          git.client
          (withFiles ["!foo.txt"] config)
          defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` True

  describe "end_of_file_fixer" $ do
    let config = defaultConfig LintRule_EndOfFileFixer

    it "succeeds when all files have correct trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` True

    it "succeeds when file is empty" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" ""
          git.exec ["add", "foo.txt"]
          runLintRules git.client config defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` True

    it "fails when file has no trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when file has multiple trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n\n\n\n"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` False

    it "autofixes when file has no trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest"
          git.exec ["add", "foo.txt"]
          report <- runLintRules git.client config defaultOptions{autofix = True, files = gitFiles ["foo.txt"]}
          readFile "foo.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "autofixes when file has multiple trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n\n\n\n"
          git.exec ["add", "foo.txt"]
          report <- runLintRules git.client config defaultOptions{autofix = True, files = gitFiles ["foo.txt"]}
          readFile "foo.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False

    it "skips files failing glob" . withGitRepo $ \git -> do
      writeFile "foo.txt" "test\ntest" >> git.exec ["add", "foo.txt"]
      report <-
        runLintRules
          git.client
          (withFiles ["!*.txt"] config)
          defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` True

  describe "no_commit_to_branch" $ do
    let mkConfig branch = withFiles [] . defaultConfig $ LintRule_NoCommitToBranch [toGlob branch]

    it "succeeds when committing on another branch" $ do
      report <-
        withGitRepo $ \git -> do
          git.exec ["switch", "-c", "test"]
          runLintRules git.client (mkConfig "main") defaultOptions
      lintReportSuccess report `shouldBe` True

    it "fails when committing on bad branch" $ do
      report <-
        withGitRepo $ \git -> do
          runLintRules git.client (mkConfig "main") defaultOptions
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "supports globs" $ do
      report <-
        withGitRepo $ \git -> do
          git.exec ["switch", "-c", "release-2.0"]
          runLintRules git.client (mkConfig "release-*") defaultOptions
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

  describe "trailing_whitespace" $ do
    let config = defaultConfig LintRule_TrailingWhitespace

    it "succeeds when no lines have trailing whitespace" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config defaultOptions{files = gitFiles ["foo.txt"]}
      lintReportSuccess report `shouldBe` True

    it "fails when line has trailing whitespace" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "end-space.txt" "test  \ntest\n"
          writeFile "end-tab.txt" "test\t\t\ntest\n"
          git.exec ["add", "end-space.txt", "end-tab.txt"]
          runLintRules git.client config $
            defaultOptions{files = gitFiles ["end-space.txt", "end-tab.txt"]}
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "autofixes trailing whitespace" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "end-space.txt" "test  \ntest\n"
          writeFile "end-tab.txt" "test\t\t\ntest\n"
          git.exec ["add", "end-space.txt", "end-tab.txt"]
          report <-
            runLintRules git.client config $
              defaultOptions
                { autofix = True
                , files = gitFiles ["end-space.txt", "end-tab.txt"]
                }
          readFile "end-space.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          readFile "end-tab.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "skips files failing glob" . withGitRepo $ \git -> do
      writeFile "bad.txt" "test  \ntest\n" >> git.exec ["add", "bad.txt"]
      report <-
        runLintRules
          git.client
          (withFiles ["!*.txt"] config)
          defaultOptions{files = gitFiles ["bad.txt"]}
      lintReportSuccess report `shouldBe` True

defaultOptions :: LintOptions
defaultOptions =
  LintOptions
    { autofix = False
    , files = mempty
    }

gitFiles :: [FilePath] -> Set GitFile
gitFiles = Set.fromList . map GitFile

defaultConfig :: LintRuleRule -> Config
defaultConfig rule =
  Config
    { repoConfigPath = ".hooky.kdl"
    , repo =
        RepoConfig
          { fileGlobs = []
          , hooks = []
          , lintRules = [LintRule rule [toGlob "*"]]
          }
    , global = error "GlobalConfig not used"
    , skippedHooks = mempty
    }

withFiles :: [Text] -> Config -> Config
withFiles globs config =
  config
    { repo =
        config.repo
          { lintRules =
              flip map config.repo.lintRules $ \rule ->
                rule{LintRule.fileGlobs = map toGlob globs}
          }
    }
