module Hooky.TestUtils (
  withTestDir,
  withGitRepo,
) where

import Path (Abs, Dir, Path)
import Path.IO (withSystemTempDir)

import Hooky.Git (GitRepo, unsafeMakeGitRepo, git_)

withTestDir :: (Path Abs Dir -> IO a) -> IO a
withTestDir = withSystemTempDir "hooky-test"

withGitRepo :: (GitRepo -> IO a) -> IO a
withGitRepo f =
  withTestDir $ \dir -> do
    let repo = unsafeMakeGitRepo dir
    git_ repo ["init"]
    git_ repo ["commit", "--allow-empty", "-m", "Initial commit"]
    f repo
