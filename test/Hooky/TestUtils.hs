module Hooky.TestUtils (
  withTestDir,
  withGitRepo,
  getPreCommitHookOutput,
) where

import Data.Text (Text)
import Path (Abs, Dir, Path)
import Path.IO (withSystemTempDir)

import Hooky.Utils.Git (GitRepo, unsafeMakeGitRepo, git, git_)
import Hooky.Utils.Process (readProcessText_)

withTestDir :: (Path Abs Dir -> IO a) -> IO a
withTestDir = withSystemTempDir "hooky-test"

withGitRepo :: (GitRepo -> IO a) -> IO a
withGitRepo f =
  withTestDir $ \dir -> do
    let repo = unsafeMakeGitRepo dir
    git_ repo ["init"]
    git_ repo ["commit", "--allow-empty", "-m", "Initial commit"]
    f repo

getPreCommitHookOutput :: GitRepo -> IO Text
getPreCommitHookOutput repo = do
  (_, stderr) <- readProcessText_ $ git repo ["commit", "--allow-empty", "-m", "test commit"]
  -- for some reason, pre-commit hook output comes out in stderr?
  return stderr
