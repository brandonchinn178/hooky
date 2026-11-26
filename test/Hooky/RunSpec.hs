{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hooky.RunSpec (spec) where

import Data.Default (def)
import Hooky.Config (RunMode (..))
import Hooky.Config qualified as RepoConfig (RepoConfig (..))
import Hooky.Internal.GitFile (GitFileTarget (..), GitFileTargetArg (..))
import Hooky.Internal.Output (OutputFormat (..))
import Hooky.Run (RunOptions (..), runHooks)
import Hooky.TestUtils.Config (toConfig, toHookConfig)
import Hooky.TestUtils.Git (withGitRepo)
import Skeletest
import Skeletest.Predicate qualified as P
import System.OsPath (osp)

spec :: Spec
spec = do
  describe "runHooks" $ do
    it "strips control characters except color" $ do
      withGitRepo $ \git -> do
        let config =
              toConfig
                def
                  { RepoConfig.hooks =
                      [ toHookConfig "colorful" $
                          [ "bash"
                          , "-c"
                          , "echo -e '\\e[31mThis is red\\e[0m'"
                          ]
                      , toHookConfig "clear-initial" $
                          [ "bash"
                          , "-c"
                          , "echo -e 'Before\\r\\e[2KAfter'"
                          ]
                      ]
                  }
        writeFile "README" ""
        runHooks git.client config runOptions
        stdout <- (.getStdout) =<< getFixture @FixtureCapturedOutput
        stdout `shouldSatisfy` P.hasInfix "│ \ESC[31mThis is red\ESC[0m"
        stdout `shouldSatisfy` P.hasInfix "│ BeforeAfter"

runOptions :: RunOptions
runOptions =
  RunOptions
    { mode = Mode_Check
    , hooksToRun = Nothing
    , fileTarget = FilesGiven [FileArg [osp|.|]]
    , format = Format_Verbose
    , stash = False
    }
