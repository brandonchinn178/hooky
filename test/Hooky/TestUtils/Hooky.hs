module Hooky.TestUtils.Hooky (
  HookyExe (..),
) where

import Skeletest
import System.Directory (findExecutable)
import System.Environment (lookupEnv)

newtype HookyExe = HookyExe FilePath

instance Fixture HookyExe where
  fixtureScope = PerSessionFixture
  fixtureAction = do
    exe <- lookupEnv "TEST_HOOKY_EXE" >>= maybe findExe pure
    pure $ noCleanup (HookyExe exe)
   where
    findExe =
      findExecutable "hooky"
        >>= maybe (error "Could not find hooky executable") pure
