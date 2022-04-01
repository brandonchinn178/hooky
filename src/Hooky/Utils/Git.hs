{-# LANGUAGE LambdaCase #-}

module Hooky.Utils.Git (
  -- * GitRepo
  GitRepo (..),
  fromGitRepo,
  unsafeMakeGitRepo,

  -- * Git operations
  getGitRepo,
  getGitPath,

  -- * Low-level
  git,
  git_,
) where

import Data.Text qualified as Text
import Path (Abs, Dir, File, Path, Rel, toFilePath)
import Path.IO (resolveDir, resolveFile)
import System.Process.Typed (ProcessConfig, proc)

import Hooky.Utils.Process (readProcessText, readProcessText_)

-- | An absolute path to a git repository.
newtype GitRepo = GitRepo (Path Abs Dir)
  deriving (Show, Eq)

fromGitRepo :: GitRepo -> Path Abs Dir
fromGitRepo (GitRepo dir) = dir

unsafeMakeGitRepo :: Path Abs Dir -> GitRepo
unsafeMakeGitRepo = GitRepo

{- |
Get the path to the root of the repository, if the given path is in a git repo.

Returns 'Nothing' if the given directory is not in a git repo.
-}
getGitRepo :: Path Abs Dir -> IO (Maybe GitRepo)
getGitRepo path = do
  (_, out, _) <- readProcessText $ proc "git" ["-C", toFilePath path, "rev-parse", "--show-toplevel"]
  case Text.unpack out of
    "" -> return Nothing
    dir -> Just . GitRepo <$> resolveDir path dir

-- | Get the given path in the .git/ directory for the given repo.
getGitPath :: ResolvePath t => GitRepo -> Path Rel t -> IO (Path Abs t)
getGitPath repo path = do
  (out, _) <- readProcessText_ $ git repo ["rev-parse", "--git-path", toFilePath path]
  resolvePath (fromGitRepo repo) (Text.unpack out)

-- | Build a git command with the given arguments in the given repo.
git :: GitRepo -> [String] -> ProcessConfig () () ()
git (GitRepo repo) args = proc "git" $ ["-C", toFilePath repo] <> args

-- | Run a git command, swallowing all stdout/stderr and throwing an error on a bad exit code.
git_ :: GitRepo -> [String] -> IO ()
git_ repo args = do
  _ <- readProcessText_ $ git repo args
  return ()

{-- Helpers --}

class ResolvePath t where
  resolvePath :: Path Abs Dir -> FilePath -> IO (Path Abs t)
instance ResolvePath File where
  resolvePath = resolveFile
instance ResolvePath Dir where
  resolvePath = resolveDir
