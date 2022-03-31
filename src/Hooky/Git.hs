{-# LANGUAGE LambdaCase #-}

module Hooky.Git (
  getGitRoot,
) where

import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (resolveDir)
import System.Process.Typed (proc, readProcess)

{- |
Get the git directory of the given directory.

Returns 'Nothing' if the given directory is not in a git directory
-}
getGitRoot :: Path Abs Dir -> IO (Maybe (Path Abs Dir))
getGitRoot path = do
  (_, out, _) <-
    readProcess $
      proc "git" ["-C", toFilePath path, "rev-parse", "--git-common-dir"]
  case TextL.unpack $ TextL.strip $ TextL.decodeUtf8 out of
    "" -> return Nothing
    dir -> Just <$> resolveDir path dir
