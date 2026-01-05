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
import System.Directory (createFileLink)

spec :: Spec
spec = do
  describe "check_broken_symlinks" $ do
    let mkConfig repo =
          LintRunConfig
            { repo = repo
            , autofix = False
            , rules = [LintRule LintRule_CheckBrokenSymlinks [toGlob "*"]]
            }

    it "succeeds on valid symlinks" $ do
      report <-
        withGitRepo $ \git -> do
          writeFile "foo.txt" "example"
          createFileLink "foo.txt" "foo-link.txt"
          git.add ["foo.txt", "foo-link.txt"]
          runLintRules (mkConfig git.repo) ["foo.txt", "foo-link.txt"]
      lintReportSuccess report `shouldBe` True

    it "fails on broken symlinks" $ do
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

    it "fails if target is deleted" $ do
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
    -- FIXME: test with `git config core.ignorecase false`
    pure ()

  describe "check_merge_conflict" $ do
    pure ()

  describe "end_of_file_fixer" $ do
    pure ()

  describe "no_commit_to_branch" $ do
    pure ()

  describe "trailing_whitespace" $ do
    pure ()
