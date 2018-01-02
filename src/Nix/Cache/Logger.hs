-- | A simple concurrent logger
module Nix.Cache.Logger (
  Logger, -- abstract
  LogLevel(..),
  withLogger,
  logAtLevel
  ) where

import System.IO (hSetBuffering, BufferMode(LineBuffering))
import Control.Concurrent (forkIO)
import qualified Data.Text.IO as T

import Nix.Cache.Common

-- | Four levels of logging.
data LogLevel
  = LOG_LOWLEVEL_DEBUG | LOG_DEBUG | LOG_INFO | LOG_WARN | LOG_FATAL
  deriving (Show, Read, Eq, Ord)

type MessageChannel = Chan (LogLevel, ThreadId, Text)

data Logger = Logger {
  -- | File handle to write messages to.
  handle :: Handle,
  -- | Minimum level to log (if it's below this, don't write to the handle).
  minLevel :: LogLevel,
  -- | Channel for messages
  messagesChannel :: MessageChannel,
  -- | Thread consuming messages
  messageConsumeThreadId :: ThreadId
  } deriving (Eq, Generic)

watchMessages :: Handle -> LogLevel -> MessageChannel -> IO ()
watchMessages handle minLevel messageChannel = forever $ do
  (level, threadId, message) <- readChan messageChannel
  when (level >= minLevel) $ do
    case level <= LOG_DEBUG of
      True -> do
        T.hPutStrLn handle $ tshow threadId <> ": " <> message
      False -> do
        T.hPutStrLn handle message

newLogger :: Handle -> LogLevel -> IO Logger
newLogger handle minLevel = do
  hSetBuffering handle LineBuffering
  messagesChannel <- newChan
  messageConsumeThreadId <- forkIO $
    watchMessages handle minLevel messagesChannel
  pure Logger {..}

stopLogger :: Logger -> IO ()
stopLogger logger = killThread (messageConsumeThreadId logger)

withLogger :: Handle -> LogLevel -> (Logger -> IO a) -> IO a
withLogger handle minLevel = bracket (newLogger handle minLevel) stopLogger

logAtLevel :: LogLevel -> Text -> Logger -> IO ()
logAtLevel level message logger = do
  tid <- myThreadId
  writeChan (messagesChannel logger) (level, tid, message)
