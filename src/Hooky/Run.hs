module Hooky.Run (
  doRun,
) where

import Hooky.Config (Config)
import Hooky.Run.ExecutionPlan (compilePlan)
import Hooky.Utils.Git (GitRepo, getStagedFiles)

doRun :: GitRepo -> Config -> IO ()
doRun repo config = do
  files <- getStagedFiles repo
  let plan = compilePlan config files
  print config
  print files
  print plan
  return ()
