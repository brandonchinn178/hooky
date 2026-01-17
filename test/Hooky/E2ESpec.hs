{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.E2ESpec (spec) where

import Data.Text (Text)
import Data.Text.IO qualified as Text
import Hooky.TestUtils.Git (withGitRepo)
import Hooky.TestUtils.Hooky (HookyExe (..))
import Skeletest
import Skeletest.Predicate qualified as P
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process qualified as Process

spec :: Spec
spec = do
  describe "hooky run" $ do
    hookyRunSpec "run"

  describe "hooky fix" $ do
    hookyRunSpec "fix"

hookyRunSpec :: String -> Spec
hookyRunSpec cmd = do
  it "defaults to --stash --staged" $ do
    withGitRepo $ \git -> do
      writeFile ".hooky.kdl" hookyConfigEofFixer
      writeFile "good.txt" "good\n"
      writeFile "bad1.txt" "bad"
      writeFile "bad2.txt" "bad\n"
      git.exec ["add", ".hooky.kdl", "good.txt", "bad2.txt"]
      writeFile "bad2.txt" "bad"
      runHooky [cmd] `shouldSatisfy` P.returns (P.eq ExitSuccess)

  it "errors if multiple file selection flags are passed" $ do
    (code, _, stderr) <- readHooky [cmd, "--all", "--modified"]
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` "hooky: Expected exactly one of: FILES, --modified, --staged, --all, --prev\n"

  it "errors if file argument and flags are both passed" $ do
    (code, _, stderr) <- readHooky [cmd, "--all", "file.txt"]
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` "hooky: Expected exactly one of: FILES, --modified, --staged, --all, --prev\n"

  it "stashes intent-to-add files" $ do
    withGitRepo $ \git -> do
      writeFile ".hooky.kdl" hookyConfigEofFixer
      writeFile "bad.txt" "bad"
      git.exec ["add", ".hooky.kdl"]
      git.exec ["add", "-N", "bad.txt"]
      let checkIntentToAdd = do
            git.client.query ["diff-files", "--name-only", "--diff-filter=A"]
              `shouldSatisfy` P.returns (P.eq "bad.txt")
            readFile "bad.txt" `shouldSatisfy` P.returns (P.eq "bad")
      checkIntentToAdd
      runHooky [cmd, "--stash", "--all"] `shouldSatisfy` P.returns (P.eq ExitSuccess)
      checkIntentToAdd

  it "stashes untracked files" $ do
    withGitRepo $ \git -> do
      writeFile ".hooky.kdl" hookyConfigEofFixer
      writeFile "bad.txt" "bad"
      git.exec ["add", ".hooky.kdl"]
      let checkUntracked = do
            git.client.query ["ls-files", "--others", "--exclude-standard"]
              `shouldSatisfy` P.returns (P.eq "bad.txt")
            readFile "bad.txt" `shouldSatisfy` P.returns (P.eq "bad")
      checkUntracked
      runHooky [cmd, "--stash", "--all"] `shouldSatisfy` P.returns (P.eq ExitSuccess)
      checkUntracked

runHooky :: [String] -> IO ExitCode
runHooky args = do
  HookyExe hooky <- getFixture
  Process.withCreateProcess (Process.proc hooky args) $ \_ _ _ h -> do
    Process.waitForProcess h

readHooky :: [String] -> IO (ExitCode, Text, Text)
readHooky args = do
  HookyExe hooky <- getFixture
  (stdout_r, stdout_w) <- Process.createPipe
  (stderr_r, stderr_w) <- Process.createPipe
  let proc =
        (Process.proc hooky args)
          { Process.std_out = Process.UseHandle stdout_w
          , Process.std_err = Process.UseHandle stderr_w
          , Process.close_fds = True
          }
  Process.withCreateProcess proc $ \_ _ _ h -> do
    code <- Process.waitForProcess h
    stdout <- Text.hGetContents stdout_r
    stderr <- Text.hGetContents stderr_r
    Text.hPutStrLn IO.stdout stdout
    Text.hPutStrLn IO.stderr stderr
    pure (code, stdout, stderr)

hookyConfigEofFixer :: String
hookyConfigEofFixer =
  """
  hook hooky {
    command hooky lint
    files *
  }
  lint_rules {
    - end_of_file_fixer
  }

  """
