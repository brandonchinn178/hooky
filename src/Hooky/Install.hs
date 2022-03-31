module Hooky.Install (
  doInstall,
) where

import Path (Abs, Dir, File, Path)

doInstall ::
  -- | The path to the 'hooky' executable to install
  Path Abs File ->
  -- | The path to the root of the git repository
  Path Abs Dir ->
  IO ()
doInstall _ _ = return ()
