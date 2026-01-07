{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.LintSpec (spec) where

import Hooky.Lint (
  LintRule (..),
  LintRuleRule (..),
  LintRunConfig (..),
  lintReportSuccess,
  renderLintReport,
  runLintRules,
  toGlob,
 )
import Hooky.TestUtils.Git (withGitRepo)
import Skeletest
import Skeletest.Predicate qualified as P
import System.Directory (createFileLink, removeFile)
import UnliftIO.Exception (SomeException)

spec :: Spec
spec = do
  describe "check_broken_symlinks" $ do
    let config =
          LintRunConfig
            { autofix = False
            , rules = [LintRule LintRule_CheckBrokenSymlinks [toGlob "*"]]
            }

    it "succeeds when all symlinks are valid" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo.txt", "foo-link.txt"]
          runLintRules git.client config ["foo.txt", "foo-link.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when a symlink is broken" $ do
      report <-
        withGitRepo $ \git -> do
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo-link.txt"]
          runLintRules git.client config ["foo-link.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when target is not tracked" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo-link.txt"]
          runLintRules git.client config ["foo-link.txt"]
      lintReportSuccess report `shouldBe` False

    it "fails when target is deleted" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.exec ["add", "foo.txt", "foo-link.txt"]
          git.exec ["commit", "-m", "Initial commit"]
          git.exec ["rm", "foo.txt"]
          runLintRules git.client config ["foo.txt"]
      lintReportSuccess report `shouldBe` False

  describe "check_case_conflict" $ do
    let config =
          LintRunConfig
            { autofix = False
            , rules = [LintRule LintRule_CheckCaseConflict [toGlob "*"]]
            }

    it "succeeds when no files conflict" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" ""
          writeFile "bar.txt" ""
          git.exec ["add", "foo.txt", "bar.txt"]
          runLintRules git.client config ["foo.txt", "bar.txt"]
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
          runLintRules git.client config ["foo.txt", "FOO.txt"]
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
          runLintRules git.client config ["FOO.txt"]
      lintReportSuccess report `shouldBe` False

  describe "check_merge_conflict" $ do
    let config =
          LintRunConfig
            { autofix = False
            , rules = [LintRule LintRule_CheckMergeConflict [toGlob "*"]]
            }

    it "succeeds when there are no merge conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "Initial commit"]
          git.exec ["switch", "-c", "branch1"] >> writeFile "foo.txt" "branch1" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch1"]
          git.exec ["switch", "-c", "branch2"] >> writeFile "bar.txt" "branch2" >> git.exec ["add", "bar.txt"] >> git.exec ["commit", "-m", "branch2"]
          git.exec ["switch", "main"]
          git.exec ["merge", "branch1", "branch2"]
          runLintRules git.client config ["foo.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when there are merge conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "Initial commit"]
          git.exec ["switch", "-c", "branch1", "main"] >> writeFile "foo.txt" "branch1" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch1"]
          git.exec ["switch", "-c", "branch2", "main"] >> writeFile "foo.txt" "branch2" >> git.exec ["add", "foo.txt"] >> git.exec ["commit", "-m", "branch2"]
          git.exec ["switch", "main"]
          git.exec ["merge", "branch1", "branch2"] `shouldSatisfy` P.throws (P.anything @SomeException)
          runLintRules git.client config ["foo.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

  describe "end_of_file_fixer" $ do
    let config =
          LintRunConfig
            { autofix = False
            , rules = [LintRule LintRule_EndOfFileFixer [toGlob "*"]]
            }

    it "succeeds when all files have correct trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config ["foo.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when file has no trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config ["foo.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when file has multiple trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n\n\n\n"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config ["foo.txt"]
      lintReportSuccess report `shouldBe` False

    it "autofixes when file has no trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest"
          git.exec ["add", "foo.txt"]
          report <- runLintRules git.client config{autofix = True} ["foo.txt"]
          readFile "foo.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "autofixes when file has multiple trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n\n\n\n"
          git.exec ["add", "foo.txt"]
          report <- runLintRules git.client config{autofix = True} ["foo.txt"]
          readFile "foo.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False

  describe "no_commit_to_branch" $ do
    let mkConfig branch =
          LintRunConfig
            { autofix = False
            , rules = [LintRule (LintRule_NoCommitToBranch [toGlob branch]) [toGlob "*"]]
            }

    it "succeeds when committing on another branch" $ do
      report <-
        withGitRepo $ \git -> do
          git.exec ["switch", "-c", "test"]
          runLintRules git.client (mkConfig "main") []
      lintReportSuccess report `shouldBe` True

    it "fails when committing on bad branch" $ do
      report <-
        withGitRepo $ \git -> do
          runLintRules git.client (mkConfig "main") []
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "supports globs" $ do
      report <-
        withGitRepo $ \git -> do
          git.exec ["switch", "-c", "release-2.0"]
          runLintRules git.client (mkConfig "release-*") []
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

  describe "trailing_whitespace" $ do
    let config =
          LintRunConfig
            { autofix = False
            , rules = [LintRule LintRule_TrailingWhitespace [toGlob "*"]]
            }

    it "succeeds when no lines have trailing whitespace" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n"
          git.exec ["add", "foo.txt"]
          runLintRules git.client config ["foo.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when line has trailing whitespace" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "end-space.txt" "test  \ntest\n"
          writeFile "end-tab.txt" "test\t\t\ntest\n"
          git.exec ["add", "end-space.txt", "end-tab.txt"]
          runLintRules git.client config ["end-space.txt", "end-tab.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "autofixes trailing whitespace" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "end-space.txt" "test  \ntest\n"
          writeFile "end-tab.txt" "test\t\t\ntest\n"
          git.exec ["add", "end-space.txt", "end-tab.txt"]
          report <- runLintRules git.client config{autofix = True} ["end-space.txt", "end-tab.txt"]
          readFile "end-space.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          readFile "end-tab.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot
