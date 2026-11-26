{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.ConfigSpec (spec) where

import Data.Default (def)
import Hooky.Config (parseRepoConfig)
import Hooky.Config qualified as HookConfig (HookConfig (..))
import Hooky.Config qualified as RepoConfig (RepoConfig (..))
import Hooky.TestUtils.Config (toHookConfig)
import Hooky.Utils.Glob (toGlob)
import Skeletest
import Skeletest.Predicate qualified as P

spec :: Spec
spec = do
  defaultsSpec
  hookSpec
  lintRulesSpec

defaultsSpec :: Spec
defaultsSpec = do
  describe "defaults" $ do
    describe "files" $ do
      it "supports specifying as args" $ do
        let input =
              """
              defaults {
                files *.hs *.md
              }
              """
            expected =
              def
                { RepoConfig.fileGlobs = [toGlob "*.hs", toGlob "*.md"]
                }
        parseRepoConfig input `shouldBe` Right expected

      it "supports specifying as list" $ do
        let input =
              """
              defaults {
                files {
                  - *.hs
                  - *.md
                }
              }
              """
            expected =
              def
                { RepoConfig.fileGlobs = [toGlob "*.hs", toGlob "*.md"]
                }
        parseRepoConfig input `shouldBe` Right expected

hookSpec :: Spec
hookSpec = do
  describe "defaults" $ do
    describe "files" $ do
      it "supports specifying as args" $ do
        let input =
              """
              hook myHook {
                command foo
                files *.hs *.md
              }
              """
            expected =
              def
                { RepoConfig.hooks =
                    [ (toHookConfig "myHook" ["foo"])
                        { HookConfig.fileGlobs = [toGlob "*.hs", toGlob "*.md"]
                        }
                    ]
                }
        parseRepoConfig input `shouldBe` Right expected

      it "supports specifying as list" $ do
        let input =
              """
              hook myHook {
                command foo
                files {
                  - *.hs
                  - *.md
                }
              }
              """
            expected =
              def
                { RepoConfig.hooks =
                    [ (toHookConfig "myHook" ["foo"])
                        { HookConfig.fileGlobs = [toGlob "*.hs", toGlob "*.md"]
                        }
                    ]
                }
        parseRepoConfig input `shouldBe` Right expected

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
    it "supports specifying 'files' with args" $ do
      parseRepoConfig ("lint_rules { - " <> rule <> " { files *.txt; }; }")
        `shouldSatisfy` P.right P.anything

    it "supports specifying 'files' with list" $ do
      parseRepoConfig ("lint_rules { - " <> rule <> " { files { - *.txt; }; }; }")
        `shouldSatisfy` P.right P.anything

  doesntSupportFiles rule = do
    it "fails if 'files' specified" $ do
      parseRepoConfig ("lint_rules { - " <> rule <> " { files *.txt; }; }")
        `shouldSatisfy` P.left (P.hasInfix "'files' config is not supported for")
