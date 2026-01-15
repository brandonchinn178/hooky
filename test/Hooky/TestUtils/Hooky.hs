module Hooky.TestUtils.Hooky (
  HookyExe (..),
) where

import Skeletest
import System.Directory (findExecutable)

newtype HookyExe = HookyExe FilePath

instance Fixture HookyExe where
  fixtureScope = PerSessionFixture
  fixtureAction = do
    exe <-
      findExecutable "hooky"
        >>= maybe (error "Could not find hooky executable") pure
    pure $ noCleanup (HookyExe exe)
