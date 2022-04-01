{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
import Path (parseAbsFile)
import Path.IO (getCurrentDir)
import System.Environment (getExecutablePath)
import System.Exit (exitFailure)

import Hooky.Install (doInstall)
import Hooky.Git (getGitRepo)
import Hooky.Run (doRun)

{-- CLI Options --}

data CLIOptions = CLIOptions
  { cliCommand :: CLICommand
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
  case cliCommand of
    CommandInstall{..} -> do
      repo <- getCurrentDir >>= getGitRepo >>= \case
        Just dir -> return dir
        Nothing -> abort "Could not install hooky: not currently in a git repository"
      exe <- getExecutablePath >>= parseAbsFile
      doInstall repo exe extraRunArgs
    CommandRun -> doRun

abort :: String -> IO a
abort s = putStrLn s >> exitFailure
