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

    it "fails when files conflict" $ do
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
    pure ()

  describe "no_commit_to_branch" $ do
    pure ()

  describe "trailing_whitespace" $ do
    pure ()
