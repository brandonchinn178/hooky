module Hooky.Utils.Process (
  runStreamedProcess,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Exit (ExitCode)
import System.IO qualified as IO
import System.Process qualified as Process

runStreamedProcess ::
  Text ->
  [Text] ->
  (Text -> IO ()) ->
  (IO.Handle -> IO ()) ->
  IO ExitCode
runStreamedProcess cmd args onOutputLine populateStdin = do
  (stdin_r, stdin_w) <- Process.createPipe
  (stdout_r, stdout_w) <- Process.createPipe
  Process.withCreateProcess
    (Process.proc (Text.unpack cmd) (map Text.unpack args))
      { Process.std_in = Process.UseHandle stdin_r
      , Process.std_out = Process.UseHandle stdout_w
      , Process.std_err = Process.UseHandle stdout_w
      , Process.close_fds = True
      }
    ( \_ _ _ h -> do
        populateStdin stdin_w :: IO ()
        IO.hClose stdin_w
        streamFrom stdout_r
        Process.waitForProcess h
    )
 where
  streamFrom h = do
    eof <- IO.hIsEOF h
    if eof
      then pure ()
      else do
        Text.hGetLine h >>= onOutputLine
        streamFrom h
