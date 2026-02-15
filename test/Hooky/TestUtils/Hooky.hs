module Hooky.TestUtils.Hooky (
  HookyExe (..),
  withHookyOnPATH,
) where

import Skeletest
import System.Directory (findExecutable)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory)
import UnliftIO.Exception (bracket)

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

withHookyOnPATH :: IO a -> IO a
withHookyOnPATH m = do
  HookyExe exe <- getFixture
  let exeDir = takeDirectory exe
  bracket (addPath exeDir) id (\_ -> m)
 where
  addPath exeDir =
    lookupEnv "PATH" >>= \case
      Just oldPATH -> do
        setEnv "PATH" (oldPATH <> ":" <> exeDir)
        pure $ setEnv "PATH" oldPATH
      Nothing -> do
        setEnv "PATH" exeDir
        pure $ unsetEnv "PATH"
