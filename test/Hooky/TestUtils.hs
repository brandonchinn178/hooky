module Hooky.TestUtils (
  withTestDir,
  withGitRepo,
  getPreCommitHookOutput,
) where

import Data.Text (Text)
import Path (Abs, Dir, Path)
import Path.IO (withSystemTempDir)

import Hooky.Utils.Git (GitRepo, git, git_, unsafeMakeGitRepo)
import Hooky.Utils.Process (readProcessText)

withTestDir :: (Path Abs Dir -> IO a) -> IO a
withTestDir = withSystemTempDir "hooky-test"

withGitRepo :: (GitRepo -> IO a) -> IO a
withGitRepo f =
  withTestDir $ \dir -> do
    let repo = unsafeMakeGitRepo dir
    git_ repo ["init"]
    git_ repo ["commit", "--allow-empty", "-m", "Initial commit"]
    f repo

getPreCommitHookOutput :: GitRepo -> [String] -> IO Text
getPreCommitHookOutput repo extraCommitArgs = do
  (_, _, stderr) <-
    readProcessText $
      git repo $
        ["commit", "--allow-empty", "-m", "test commit"] <> extraCommitArgs

  -- for some reason, pre-commit hook output comes out in stderr?
  return stderr
