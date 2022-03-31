module Hooky.TestUtils (
  withTestDir,
  withGitDir,
) where

import Path (Abs, Dir, Path)
import Path.IO (withSystemTempDir)

import Hooky.Git (git_)

withTestDir :: (Path Abs Dir -> IO a) -> IO a
withTestDir = withSystemTempDir "hooky-test"

withGitDir :: (Path Abs Dir -> IO a) -> IO a
withGitDir f =
  withTestDir $ \dir -> do
    git_ dir ["init"]
    git_ dir ["commit", "--allow-empty", "-m", "Initial commit"]
    f dir
