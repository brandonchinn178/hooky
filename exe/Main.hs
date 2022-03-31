{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Options.Applicative
import Path (parseAbsFile)
import Path.IO (getCurrentDir)
import System.Environment (getExecutablePath)
import System.Exit (exitFailure)

import Hooky.Install (doInstall)
import Hooky.Git (getGitRoot)
import Hooky.Run (doRun)

{-- CLI Options --}

data CLIOptions = CLIOptions
  { cliCommand :: CLICommand
  }

data CLICommand = CommandInstall | CommandRun

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
    parseInstall = pure CommandInstall
    parseRun = pure CommandRun

{-- Entrypoint --}

main :: IO ()
main = do
  CLIOptions{..} <- execParser cliOptions
  case cliCommand of
    CommandInstall -> do
      exe <- getExecutablePath >>= parseAbsFile
      cwd <- getCurrentDir >>= getGitRoot >>= \case
        Just dir -> return dir
        Nothing -> abort "Could not install hooky: not currently in a git repository"
      doInstall exe cwd
    CommandRun -> doRun

abort :: String -> IO a
abort s = putStrLn s >> exitFailure
