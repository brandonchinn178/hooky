{-# LANGUAGE LambdaCase #-}

module Hooky.Git (
  getGitRoot,

  -- * Low-level
  git,
  git_,
) where

import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (resolveDir)
import System.Process.Typed (ProcessConfig, proc, readProcess, readProcess_)

{- |
Get the git directory of the given directory.

Returns 'Nothing' if the given directory is not in a git directory
-}
getGitRoot :: Path Abs Dir -> IO (Maybe (Path Abs Dir))
getGitRoot path = do
  (_, out, _) <- readProcess $ git path ["rev-parse", "--git-common-dir"]
  case TextL.unpack $ TextL.strip $ TextL.decodeUtf8 out of
    "" -> return Nothing
    dir -> Just <$> resolveDir path dir

-- | Build a git command with the given arguments in the given directory.
git :: Path b Dir -> [String] -> ProcessConfig () () ()
git dir args = proc "git" $ ["-C", toFilePath dir] <> args

-- | Run a git command, swallowing all stdout/stderr and throwing an error on a bad exit code.
git_ :: Path b Dir -> [String] -> IO ()
git_ dir args = do
  _ <- readProcess_ $ git dir args
  return ()
