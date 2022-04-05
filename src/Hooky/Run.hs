module Hooky.Run (
  doRun,
) where

import Hooky.Config (Config)

doRun :: Config -> IO ()
doRun config = do
  print config
  return ()
