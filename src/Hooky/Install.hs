{-# LANGUAGE QuasiQuotes #-}

module Hooky.Install (
  doInstall,
) where

import Path (Abs, Dir, File, Path, relfile, toFilePath, (</>))
import Path.IO (getPermissions, setPermissions, setOwnerExecutable)

doInstall ::
  -- | The path to the 'hooky' executable to install
  Path Abs File ->
  -- | The path to the root of the git repository
  Path Abs Dir ->
  IO ()
doInstall hookyExe gitDir = do
  writeFile (toFilePath preCommitPath) . unlines $
    [ "#!/usr/bin/env sh"
    , "exec " <> toFilePath hookyExe <> " run"
    ]
  setPermissions preCommitPath . setOwnerExecutable True =<< getPermissions preCommitPath
  where
    preCommitPath = gitDir </> [relfile|hooks/pre-commit|]
