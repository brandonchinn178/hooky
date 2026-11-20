{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Utils.Process (
  -- * Running processes; meant to be imported qualified
  Process,
  StreamOut (..),
  new,
  run,
  setStdin,
  setStderr,
  getOutput,
  streamOutput,

  -- * Utilities
  renderShell,
) where

import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hooky.Error (HookyError (..))
import Hooky.Internal.Logging qualified as Logging
import Hooky.Utils.OsPath qualified as OsPath
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.OsString (OsString)
import System.Process qualified as Process
import UnliftIO.Async (withAsync)
import UnliftIO.Async qualified as Async

{----- Running processes -----}

data Process a = Process
  { cmd :: OsString
  , args :: [OsString]
  , populateStdin :: IO.Handle -> IO ()
  , stderr :: StreamOut
  , callback :: Process.ProcessHandle -> IO.Handle -> IO.Handle -> IO a
  }

data StreamOut
  = StreamStdout
  | StreamStderr

new :: OsString -> [OsString] -> Process ExitCode
new cmd args =
  Process
    { cmd
    , args
    , populateStdin = \_ -> pure ()
    , stderr = StreamStderr
    , callback = \h _ _ -> Process.waitForProcess h
    }

run :: Process a -> IO a
run proc = do
  Logging.debug $ "Running proc: " <> Text.show (proc.cmd : proc.args)

  (stdin_r, stdin_w) <- Process.createPipe
  (stdout_r, stdout_w) <- Process.createPipe
  (stderr_r, stderr_w) <-
    case proc.stderr of
      StreamStdout -> pure (stdout_r, stdout_w)
      StreamStderr -> Process.createPipe
  IO.hSetEncoding stdout_r IO.utf8
  IO.hSetEncoding stderr_r IO.utf8

  Process.withCreateProcess
    (Process.proc (OsPath.toFilePath proc.cmd) (map OsPath.toFilePath proc.args))
      { Process.std_in = Process.UseHandle stdin_r
      , Process.std_out = Process.UseHandle stdout_w
      , Process.std_err = Process.UseHandle stderr_w
      , Process.close_fds = True
      }
    ( \_ _ _ h -> do
        proc.populateStdin stdin_w
        IO.hClose stdin_w
        proc.callback h stdout_r stderr_r
    )

setStdin :: LazyByteString -> Process a -> Process a
setStdin bs proc = proc{populateStdin = \h -> LazyByteString.hPutStr h bs}

setStderr :: StreamOut -> Process a -> Process a
setStderr stream proc = proc{stderr = stream}

getOutput :: Process a -> Process (Either HookyError Text)
getOutput proc = proc{callback}
 where
  callback h stdoutH stderrH = do
    -- Fork a thread to consume the pipes, so they don't get blocked
    withAsync (Text.hGetContents stdoutH) $ \stdoutAsync -> do
      Async.link stdoutAsync
      withAsync (Text.hGetContents stderrH) $ \stderrAsync -> do
        Async.link stderrAsync

        code <- Process.waitForProcess h
        stdout <- Async.wait stdoutAsync
        stderr <- Async.wait stderrAsync
        case code of
          ExitSuccess -> do
            pure $ Right stdout
          ExitFailure n -> do
            pure . Left . HookyError . Text.unlines $
              [ "command exited with code " <> Text.show n <> ": " <> Text.show (proc.cmd : proc.args)
              , stderr
              ]

streamOutput :: (Text -> IO ()) -> Process a -> Process a
streamOutput onOutputLine proc = proc{callback}
 where
  callback h stdoutH stderrH = do
    streamFrom stdoutH
    proc.callback h stdoutH stderrH
  streamFrom h = do
    eof <- IO.hIsEOF h
    if eof
      then pure ()
      else do
        Text.hGetLine h >>= onOutputLine
        streamFrom h

{----- Utilities -----}

renderShell :: [Text] -> Text
renderShell args =
  Text.intercalate " " $
    [ if Text.any isSpace s then "'" <> s <> "'" else s
    | s <- args
    ]
