{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Internal.Logging (
  initialize,
  LogLevel (..),
  Logger,

  -- * Log methods
  debug,
  info,
  warn,
  error,

  -- * Impure log methods
  traceDebug,
  traceInfo,
  traceWarn,
  traceError,
) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.IO qualified as TextL
import Hooky.Utils.Term qualified as Term
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (error)

data LogLevel
  = LogLevel_Debug
  | LogLevel_Info
  | LogLevel_Warn
  | LogLevel_Error
  deriving (Show, Eq, Ord)

data Logger = Logger
  { level :: LogLevel
  }

newLogger :: LogLevel -> Logger
newLogger level = Logger{level}

loggerRef :: IORef Logger
loggerRef = unsafePerformIO $ newIORef (newLogger LogLevel_Warn)
{-# OPAQUE loggerRef #-}

initialize :: LogLevel -> IO ()
initialize = writeIORef loggerRef . newLogger

debug :: Text -> IO ()
debug = sendLog LogLevel_Debug

info :: Text -> IO ()
info = sendLog LogLevel_Info

warn :: Text -> IO ()
warn = sendLog LogLevel_Warn

error :: Text -> IO ()
error = sendLog LogLevel_Error

sendLog :: LogLevel -> Text -> IO ()
sendLog level msg = do
  logger <- readIORef loggerRef
  when (level >= logger.level) $ do
    TextL.putStrLn . levelColor level . TextL.fromStrict $ showLevel level <> msg
 where
  showLevel = \case
    LogLevel_Debug -> "[debug] "
    LogLevel_Info -> "[info] "
    LogLevel_Warn -> "[warn] "
    LogLevel_Error -> "[error] "
  levelColor = \case
    LogLevel_Debug -> Term.gray
    LogLevel_Info -> Term.gray
    LogLevel_Warn -> Term.yellow
    LogLevel_Error -> Term.red

traceDebug :: Text -> a -> a
traceDebug = sendTrace LogLevel_Debug

traceInfo :: Text -> a -> a
traceInfo = sendTrace LogLevel_Info

traceWarn :: Text -> a -> a
traceWarn = sendTrace LogLevel_Warn

traceError :: Text -> a -> a
traceError = sendTrace LogLevel_Error

sendTrace :: LogLevel -> Text -> a -> a
sendTrace level msg a = unsafePerformIO $ sendLog level msg >> pure a
