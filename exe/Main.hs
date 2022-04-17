{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad ((>=>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative hiding (action)
import Path (Abs, Dir, File, Path, parseAbsFile, toFilePath)
import Path.IO (getCurrentDir, resolveFile)
import System.Environment (getExecutablePath)
import System.Exit (exitFailure)

import Hooky.Config (Config, parseConfig)
import Hooky.Install (doInstall)
import Hooky.Run (doRun)
import Hooky.Utils.Git (GitRepo, getGitRepo)

{-- CLI Options --}

data CLIOptions = CLIOptions
  { cliCommand :: CLICommand
  , cliConfigFilePath :: FilePath
  }

data CLICommand
  = CommandInstall
      { extraRunArgs :: [Text]
      }
  | CommandRun

cliOptions :: ParserInfo CLIOptions
cliOptions =
  info (helper <*> parseOptions) . mconcat $
    [ fullDesc
    , header "Hooky: A fully customizable git hooks manager"
    ]
  where
    parseOptions =
      CLIOptions
        <$> parseCommand
        <*> parseConfigFilePath

    parseConfigFilePath =
      strOption . mconcat $
        [ long "config"
        , short 'c'
        , help "Path to config file"
        , value "hooky-config.yaml"
        , showDefault
        ]

    parseCommand =
      hsubparser . mconcat $
        [ command "install" (info parseInstall $ progDesc "Install hooky as the git pre-commit hook")
        , command "run" (info parseRun $ progDesc "Run hooks manually")
        ]
    parseInstall =
      CommandInstall
        <$> parseInstallExtraRunArgs
    parseInstallExtraRunArgs =
      option (Text.splitOn " " <$> str) . mconcat $
        [ long "run-args"
        , help "Extra arguments to pass to the 'hooky run' command in the pre-commit hook"
        , value []
        ]
    parseRun = pure CommandRun

{-- Entrypoint --}

main :: IO ()
main = do
  CLIOptions{..} <- execParser cliOptions
  cwd <- getCurrentDir
  configFile <- resolveFile cwd cliConfigFilePath

  case cliCommand of
    CommandInstall{..} -> do
      repo <- getGitRepoOrFail "install" cwd
      exe <- getExecutablePath >>= parseAbsFile
      let args = ["--config", Text.pack (toFilePath configFile)] <> extraRunArgs
      doInstall repo exe args
    CommandRun -> do
      repo <- getGitRepoOrFail "run" cwd
      config <- readConfig configFile
      doRun repo config

getGitRepoOrFail :: String -> Path Abs Dir -> IO GitRepo
getGitRepoOrFail action cwd =
  getGitRepo cwd >>= \case
    Just repo -> return repo
    Nothing -> abort $ "Could not " <> action <> " hooky: not currently in a git repository"

readConfig :: Path Abs File -> IO Config
readConfig = readConfigFile >=> decodeConfig
  where
    readConfigFile = Text.readFile . toFilePath
    decodeConfig = either (abort . Text.unpack) return . parseConfig

abort :: String -> IO a
abort s = putStrLn s >> exitFailure
