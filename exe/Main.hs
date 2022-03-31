{-# LANGUAGE RecordWildCards #-}

import Options.Applicative

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
    CommandInstall -> doInstall
    CommandRun -> doRun

doInstall :: IO ()
doInstall = putStrLn "TODO: <install>"

doRun :: IO ()
doRun = putStrLn "TODO: <run>"
