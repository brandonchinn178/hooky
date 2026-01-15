{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hooky.E2ESpec (spec) where

import Hooky.TestUtils.Git (withGitRepo)
import Hooky.TestUtils.Hooky (HookyExe (..))
import Skeletest
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

spec :: Spec
spec = do
  describe "hooky run" $ do
    hookyRunArgsSpec "run"

  describe "hooky fix" $ do
    hookyRunArgsSpec "fix"

hookyRunArgsSpec :: String -> Spec
hookyRunArgsSpec cmd = do
  it "defaults to --stash --staged" $ do
    HookyExe hooky <- getFixture @HookyExe
    withGitRepo $ \git -> do
      writeFile ".hooky.kdl" $
        """
        hook hooky {
          command hooky lint
          files *
        }
        lint_rules {
          - end_of_file_fixer
        }

        """
      writeFile "good.txt" "good\n"
      writeFile "bad1.txt" "bad"
      writeFile "bad2.txt" "bad\n"
      git.exec ["add", ".hooky.kdl", "good.txt", "bad2.txt"]
      writeFile "bad2.txt" "bad"
      (code, stdout, stderr) <- readProcessWithExitCode hooky [cmd] ""
      context (unlines [stdout, stderr]) $
        code `shouldBe` ExitSuccess

  it "errors if multiple file selection flags are passed" $ do
    HookyExe hooky <- getFixture @HookyExe
    (code, stdout, stderr) <- readProcessWithExitCode hooky [cmd, "--all", "--modified"] ""
    context (unlines [stdout, stderr]) $ do
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` "hooky: Expected exactly one of: FILES, --modified, --staged, --all, --prev\n"

  it "errors if file argument and flags are both passed" $ do
    HookyExe hooky <- getFixture @HookyExe
    (code, stdout, stderr) <- readProcessWithExitCode hooky [cmd, "--all", "file.txt"] ""
    context (unlines [stdout, stderr]) $ do
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` "hooky: Expected exactly one of: FILES, --modified, --staged, --all, --prev\n"
