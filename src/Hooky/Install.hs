{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.Install (
  doInstall,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Path (Abs, File, Path, relfile)

import Hooky.Utils.Git (GitRepo, getGitPath)
import Hooky.Utils.Path (pathToText, writeScript)

doInstall ::
  -- | The path to the git repository
  GitRepo ->
  -- | The path to the 'hooky' executable to install
  Path Abs File ->
  -- | Extra arguments to pass to the 'hooky run' command
  [Text] ->
  IO ()
doInstall repo hookyExe extraRunArgs = do
  preCommitPath <- getGitPath repo [relfile|hooks/pre-commit|]
  writeScript preCommitPath . Text.unlines $
    [ "#!/usr/bin/env sh"
    , "exec " <> quote (pathToText hookyExe) <> " run " <> args
    ]
  where
    quote s = "'" <> s <> "'"
    args = Text.unwords $ map quote extraRunArgs
