{-# LANGUAGE OverloadedStrings #-}

module Hooky.ConfigSpec (spec) where

import Hooky.Config (
  parseRepoConfig,
 )
import Skeletest
import Skeletest.Predicate qualified as P

spec :: Spec
spec = do
  lintRulesSpec

lintRulesSpec :: Spec
lintRulesSpec = do
  describe "lint_rules" $ do
    describe "check_broken_symlinks" $ do
      supportsFiles "check_broken_symlinks"

    describe "check_case_conflict" $ do
      supportsFiles "check_case_conflict"

    describe "check_merge_conflict" $ do
      supportsFiles "check_merge_conflict"

    describe "end_of_file_fixer" $ do
      supportsFiles "end_of_file_fixer"

    describe "no_commit_to_branch" $ do
      doesntSupportFiles "no_commit_to_branch"

    describe "trailing_whitespace" $ do
      supportsFiles "trailing_whitespace"
 where
  supportsFiles rule = do
    it "supports 'files' configuration" $ do
      parseRepoConfig ("lint_rules { - " <> rule <> " { files *.txt; }; }")
        `shouldSatisfy` P.right P.anything

  doesntSupportFiles rule = do
    it "fails if 'files' specified" $ do
      parseRepoConfig ("lint_rules { - " <> rule <> " { files *.txt; }; }")
        `shouldSatisfy` P.left (P.hasInfix "'files' config is not supported for")
