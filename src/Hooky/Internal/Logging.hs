{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hooky.Internal.Logging (
  initialize,
  LogLevel (..),
  Logger,
  debug,
  info,
  warn,
  error,
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
    TextL.putStrLn . Term.gray . TextL.fromStrict $ msg
