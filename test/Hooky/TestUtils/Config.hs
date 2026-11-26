{-# LANGUAGE OverloadedStrings #-}

module Hooky.TestUtils.Config (
  toConfig,
  toHookConfig,
) where

import Data.Default (def)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Hooky.Config (Config, HookConfig (..), PassFilesMode (..), RepoConfig)
import Hooky.Config qualified as Config (Config (..))
import Hooky.Config qualified as GlobalConfig (GlobalConfig (..))
import Hooky.Utils.Glob (toGlob)

toConfig :: RepoConfig -> Config
toConfig repoConfig =
  def
    { Config.global =
        def
          { GlobalConfig.maxParallelHooks = 1
          }
    , Config.repo = repoConfig
    }

toHookConfig :: Text -> [Text] -> HookConfig
toHookConfig name args =
  HookConfig
    { name
    , cmdArgs = NonEmpty.fromList args
    , checkArgs = []
    , fixArgs = []
    , passFiles = PassFiles_XArgs
    , fileGlobs = [toGlob "*"]
    }
