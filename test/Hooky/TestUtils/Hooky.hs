module Hooky.TestUtils.Hooky (
  HookyExe (..),
) where

import Skeletest
import System.Directory (findExecutable)

newtype HookyExe = HookyExe FilePath

instance Fixture HookyExe where
  fixtureScope = PerSessionFixture
  fixtureAction = do
    exe <- findExe
    pure $ noCleanup (HookyExe exe)
   where
    findExe =
      findExecutable "hooky"
        >>= maybe (error "Could not find hooky executable") pure
