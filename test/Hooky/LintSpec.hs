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
import Hooky.TestUtils.Git (GitRepo (..), withGitRepo)
import Skeletest
import Skeletest.Predicate qualified as P
import System.Directory (createFileLink, removeFile)
import UnliftIO.Exception (SomeException)

spec :: Spec
spec = do
  describe "check_broken_symlinks" $ do
    let mkConfig repo =
          LintRunConfig
            { repo = repo
            , autofix = False
            , rules = [LintRule LintRule_CheckBrokenSymlinks [toGlob "*"]]
            }

    it "succeeds when all symlinks are valid" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.add ["foo.txt", "foo-link.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt", "foo-link.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when a symlink is broken" $ do
      report <-
        withGitRepo $ \git -> do
          createFileLink "foo.txt" "foo-link.txt"
          git.add ["foo-link.txt"]
          runLintRules (mkConfig git.repo) ["foo-link.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when target is not tracked" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.add ["foo-link.txt"]
          runLintRules (mkConfig git.repo) ["foo-link.txt"]
      lintReportSuccess report `shouldBe` False

    it "fails when target is deleted" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.add ["foo.txt", "foo-link.txt"]
          git.commit "Initial commit"
          git.rm ["foo.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt"]
      lintReportSuccess report `shouldBe` False

  describe "check_case_conflict" $ do
    let mkConfig repo =
          LintRunConfig
            { repo = repo
            , autofix = False
            , rules = [LintRule LintRule_CheckCaseConflict [toGlob "*"]]
            }

    it "succeeds when no files conflict" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" ""
          writeFile "bar.txt" ""
          git.add ["foo.txt", "bar.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt", "bar.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when files conflict" $ do
      report <-
        withGitRepo $ \git -> do
          git.run ["config", "core.ignorecase", "false"]
          writeFile "foo.txt" ""
          git.add ["foo.txt"]
          removeFile "foo.txt"
          writeFile "FOO.TXT" ""
          git.add ["FOO.TXT"]
          git.run ["checkout", "foo.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt", "FOO.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when new file conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          git.run ["config", "core.ignorecase", "false"]
          writeFile "foo.txt" ""
          git.add ["foo.txt"]
          git.commit "Initial commit"
          removeFile "foo.txt"
          writeFile "FOO.TXT" ""
          git.add ["FOO.TXT"]
          git.run ["checkout", "foo.txt"]
          runLintRules (mkConfig git.repo) ["FOO.txt"]
      lintReportSuccess report `shouldBe` False

  describe "check_merge_conflict" $ do
    let mkConfig repo =
          LintRunConfig
            { repo = repo
            , autofix = False
            , rules = [LintRule LintRule_CheckMergeConflict [toGlob "*"]]
            }

    it "succeeds when there are no merge conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "" >> git.add ["foo.txt"] >> git.commit "Initial commit"
          git.run ["switch", "-c", "branch1"] >> writeFile "foo.txt" "branch1" >> git.add ["foo.txt"] >> git.commit "branch1"
          git.run ["switch", "-c", "branch2"] >> writeFile "bar.txt" "branch2" >> git.add ["bar.txt"] >> git.commit "branch2"
          git.run ["switch", "main"]
          git.run ["merge", "branch1", "branch2"]
          runLintRules (mkConfig git.repo) ["foo.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when there are merge conflicts" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "" >> git.add ["foo.txt"] >> git.commit "Initial commit"
          git.run ["switch", "-c", "branch1", "main"] >> writeFile "foo.txt" "branch1" >> git.add ["foo.txt"] >> git.commit "branch1"
          git.run ["switch", "-c", "branch2", "main"] >> writeFile "foo.txt" "branch2" >> git.add ["foo.txt"] >> git.commit "branch2"
          git.run ["switch", "main"]
          git.run ["merge", "branch1", "branch2"] `shouldSatisfy` P.throws (P.anything @_ @SomeException)
          runLintRules (mkConfig git.repo) ["foo.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

  describe "end_of_file_fixer" $ do
    let mkConfig repo =
          LintRunConfig
            { repo = repo
            , autofix = False
            , rules = [LintRule LintRule_EndOfFileFixer [toGlob "*"]]
            }

    it "succeeds when all files have correct trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n"
          git.add ["foo.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails when file has no trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest"
          git.add ["foo.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt"]
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "fails when file has multiple trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n\n\n\n"
          git.add ["foo.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt"]
      lintReportSuccess report `shouldBe` False

    it "autofixes when file has no trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest"
          git.add ["foo.txt"]
          report <- runLintRules (mkConfig git.repo){autofix = True} ["foo.txt"]
          readFile "foo.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "autofixes when file has multiple trailing newlines" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "test\ntest\n\n\n\n"
          git.add ["foo.txt"]
          report <- runLintRules (mkConfig git.repo){autofix = True} ["foo.txt"]
          readFile "foo.txt" `shouldSatisfy` P.returns (P.eq "test\ntest\n")
          pure report
      lintReportSuccess report `shouldBe` False

  describe "no_commit_to_branch" $ do
    let mkConfig repo branch =
          LintRunConfig
            { repo = repo
            , autofix = False
            , rules = [LintRule (LintRule_NoCommitToBranch [toGlob branch]) [toGlob "*"]]
            }

    it "succeeds when committing on another branch" $ do
      report <-
        withGitRepo $ \git -> do
          git.run ["switch", "-c", "test"]
          runLintRules (mkConfig git.repo "main") []
      lintReportSuccess report `shouldBe` True

    it "fails when committing on bad branch" $ do
      report <-
        withGitRepo $ \git -> do
          runLintRules (mkConfig git.repo "main") []
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

    it "supports globs" $ do
      report <-
        withGitRepo $ \git -> do
          git.run ["switch", "-c", "release-2.0"]
          runLintRules (mkConfig git.repo "release-*") []
      lintReportSuccess report `shouldBe` False
      renderLintReport report `shouldSatisfy` P.matchesSnapshot

  describe "trailing_whitespace" $ do
    pure ()
