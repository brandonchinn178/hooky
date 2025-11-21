{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (unless)
-- import Data.List.NonEmpty qualified as NonEmpty
-- import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative hiding (action)
-- import Options.Applicative.NonEmpty (some1)
-- import Path (Abs, Dir, File, Path, parseAbsFile, toFilePath)
-- import Path.IO (doesFileExist, getCurrentDir, resolveFile)
-- import System.Environment (getExecutablePath)
import System.Directory (doesFileExist, makeAbsolute)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import Hooky.Config (parseConfig)
-- import Hooky.Install (doInstall)
-- import Hooky.Run (RunOptions (..), doRun)
-- import Hooky.Utils.Git (GitRepo, getGitRepo)

{-- CLI Options --}

data CLIOptions = CLIOptions
  { cliCommand :: CLICommand
  , cliConfigFilePath :: FilePath
  }

data CLICommand
  = CommandInstall
  | CommandRun
  | CommandFix
  | CommandLint

cliOptions :: ParserInfo CLIOptions
cliOptions =
  info (helper <*> parseOptions) . mconcat $
    [ fullDesc
    , header "Hooky: A minimal git hooks manager"
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
        , value ".hooky.kdl"
        , showDefault
        ]

    parseCommand =
      hsubparser . mconcat $
        [ command "install" (info parseInstall $ progDesc "Install hooky as the git pre-commit hook")
        , command "run" (info parseRun $ progDesc "Run hooks")
        , command "fix" (info parseFix $ progDesc "Run hooks with autofixing enabled")
        , command "lint" (info parseLint $ progDesc "Run builtin hooky rules")
        ]

    parseInstall = pure CommandInstall
    parseRun = pure CommandRun
    parseFix = pure CommandFix
    parseLint = pure CommandLint

{-- Entrypoint --}

main :: IO ()
main = do
  CLIOptions{..} <- execParser cliOptions

  repo <- readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] "" >>= \case
    (ExitFailure _, _, _) -> abort "hooky: not currently in a git repository"
    (ExitSuccess, stdout, _) -> pure stdout

  configFile <- makeAbsolute cliConfigFilePath
  configFileExists <- doesFileExist configFile
  unless configFileExists $
    abort $ "Config file doesn't exist: " <> configFile

  config <- either (abort . Text.unpack) pure . parseConfig =<< Text.readFile configFile

  print repo
  print config

  -- case cliCommand of
  --   CommandInstall -> do
  --     exe <- getExecutablePath >>= parseAbsFile
  --     let args = ["--config", Text.pack (toFilePath configFile)]
  --     doInstall repo exe args
  --   CommandRun -> do
  --     config <- loadConfig configFile
  --     success <-
  --       doRun repo config $
  --         RunOptions
  --           { showStdoutOnSuccess = cliLogLevel >= Verbose
  --           }
  --     unless success exitFailure

abort :: String -> IO a
abort s = hPutStrLn stderr s >> exitFailure
