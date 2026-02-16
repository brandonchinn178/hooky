{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hooky.E2ESpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hooky.Internal.Output (allOutputFormats, renderOutputFormat)
import Hooky.TestUtils.Git (withGitRepo)
import Hooky.TestUtils.Hooky (HookyExe (..))
import Skeletest
import Skeletest.Predicate qualified as P
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.Process qualified as Process

spec :: Spec
spec = do
  describe "git commit" $ do
    it "runs hooky" . withGitRepo $ \git -> do
      Process.callProcess "bash" ["-c", "env | grep PATH"]
      writeFile ".hooky.kdl" hookyConfigEofFixer
      writeFile "good.txt" "good\n"
      writeFile "bad.txt" "bad"
      runHooky ["install"] `shouldSatisfy` P.returns (P.eq ExitSuccess)
      git.exec ["add", ".hooky.kdl", "good.txt", "bad.txt"]
      (code0, _, stderr0) <- git.run ["commit", "-m", "initial commit"]
      code0 `shouldBe` ExitFailure 1
      stderr0 `shouldSatisfy` P.hasInfix "1 hook failed"

      writeFile "bad.txt" "bad\n"
      git.exec ["add", "bad.txt"]
      (code1, _, stderr1) <- git.run ["commit", "-m", "commit"]
      code1 `shouldBe` ExitSuccess
      stderr1 `shouldSatisfy` P.hasInfix "1 hook passed"

  describe "hooky run" $ do
    it "defaults to --stash --staged" $ do
      withGitRepo $ \git -> do
        writeFile ".hooky.kdl" hookyConfigEofFixer
        writeFile "good.txt" "good\n"
        writeFile "bad1.txt" "bad"
        writeFile "bad2.txt" "bad\n"
        git.exec ["add", ".hooky.kdl", "good.txt", "bad2.txt"]
        writeFile "bad2.txt" "bad"
        runHooky ["run"] `shouldSatisfy` P.returns (P.eq ExitSuccess)

    it "errors if multiple file selection flags are passed" $ do
      (code, _, stderr) <- readHooky ["run", "--all", "--modified"]
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` "Expected exactly one of: FILES, --modified, --staged, --all, --prev\n"

    it "errors if file argument and flags are both passed" $ do
      (code, _, stderr) <- readHooky ["run", "--all", "file.txt"]
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` "Expected exactly one of: FILES, --modified, --staged, --all, --prev\n"

    it "stashes intent-to-add files" $ do
      withGitRepo $ \git -> do
        writeFile ".hooky.kdl" hookyConfigEofFixer
        writeFile "bad.txt" "bad"
        git.exec ["add", ".hooky.kdl"]
        git.exec ["add", "-N", "bad.txt"]
        let checkIntentToAdd = do
              (_, stdout, _) <- git.run ["diff-files", "--name-only", "--diff-filter=A"]
              stdout `shouldBe` "bad.txt"
              readFile "bad.txt" `shouldSatisfy` P.returns (P.eq "bad")
        checkIntentToAdd
        runHooky ["run", "--stash", "--all"] `shouldSatisfy` P.returns (P.eq ExitSuccess)
        checkIntentToAdd

    it "stashes untracked files" $ do
      withGitRepo $ \git -> do
        writeFile ".hooky.kdl" hookyConfigEofFixer
        writeFile "bad.txt" "bad"
        git.exec ["add", ".hooky.kdl"]
        let checkUntracked = do
              (_, stdout, _) <- git.run ["ls-files", "--others", "--exclude-standard"]
              stdout `shouldBe` "bad.txt"
              readFile "bad.txt" `shouldSatisfy` P.returns (P.eq "bad")
        checkUntracked
        runHooky ["run", "--stash", "--all"] `shouldSatisfy` P.returns (P.eq ExitSuccess)
        checkUntracked

    it "error if .hooky.kdl is to be stashed" $ do
      withGitRepo $ \git -> do
        writeFile ".hooky.kdl" hookyConfigEofFixer
        git.exec ["add", ".hooky.kdl"]
        git.exec ["commit", "-m", "test"]
        writeFile ".hooky.kdl" $ hookyConfigEofFixer <> "\n\n\n"
        (code, _, stderr) <- readHooky ["run", "--stash", "--staged"]
        code `shouldBe` ExitFailure 1
        stderr `shouldBe` ".hooky.kdl has changes, stage it first\n"

    forM_ allOutputFormats $ \format -> do
      let flag = Text.unpack $ "--format=" <> renderOutputFormat format
          scrubDuration s =
            case Text.breakOn "duration: " s of
              (_, "") -> s
              (pre, post) -> pre <> "duration: XX.XXXs" <> scrubDuration (Text.dropWhile (/= '\n') post)
      -- https://github.com/brandonchinn178/hooky/issues/9
      skip "until we can set --max-parallel-hooks 1" . it ("outputs " <> flag) $ do
        (code, stdout, _) <-
          withGitRepo $ \git -> do
            writeFile ".hooky.kdl" $
              """
              hook hooky_pass {
                command hooky lint
                files *.pass
              }
              hook hooky_fail {
                command hooky lint
                files *.fail
              }
              hook hooky_skip {
                command hooky lint
                files *.skip
              }
              lint_rules {
                - end_of_file_fixer
              }

              """
            writeFile "file.pass" "good\n"
            writeFile "file.fail" "bad"
            git.exec ["add", "."]
            readHooky ["run", "--all", flag]
        code `shouldBe` ExitFailure 1
        scrubDuration stdout `shouldSatisfy` P.matchesSnapshot

    describe "skipping hooks with env var" $ do
      let runHookyWithEnv env args = do
            HookyExe hooky <- getFixture
            Process.withCreateProcess (Process.proc "env" $ [env, hooky] <> args) $ \_ _ _ h -> do
              Process.waitForProcess h
      it "skips entire hook" $ do
        withGitRepo $ \git -> do
          writeFile ".hooky.kdl" $
            """
            hook hooky1 {
              command hooky lint
              files *.good
            }
            hook hooky2 {
              command hooky lint
              files *.bad
            }
            lint_rules {
              - end_of_file_fixer
            }

            """
          writeFile "test.good" "test\n"
          writeFile "test.bad" "test"
          git.exec ["add", "."]
          runHookyWithEnv "SKIP=hooky2" ["run", "--all"] `shouldSatisfy` P.returns (P.eq ExitSuccess)

      it "skips hooky lint rules" $ do
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
          writeFile "test.txt" "test"
          git.exec ["add", "."]
          runHookyWithEnv "SKIP=end_of_file_fixer" ["run", "--all"] `shouldSatisfy` P.returns (P.eq ExitSuccess)

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
  IO.hSetEncoding stdout_r IO.utf8
  IO.hSetEncoding stderr_r IO.utf8
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
    pure (code, stripControlChars stdout, stripControlChars stderr)
 where
  stripControlChars s =
    case Text.breakOn "\x1b" s of
      (_, "") -> s
      (pre, post) -> pre <> stripControlChars (Text.drop 1 . Text.dropWhile (/= 'm') $ post)

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
