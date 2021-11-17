{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Logger
     ( withLogger
     , logInfo
     , logError
     , logWarning
     , MyLoggerMonad
     , MonadLogger

     -- * Utils
     , loggedFail
     ) where

import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

import Data.Function (fix)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)

import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, wait, asyncBound)
import UnliftIO.Exception (SomeException, finally)
import UnliftIO.MVar (MVar, takeMVar, putMVar, newEmptyMVar)

import Control.Monad.Logger
  ( MonadLogger (..)
  , LogLevel (..)
  , Loc (..)
  , LogLine
  , fromLogStr
  , logInfoCS
  , logErrorCS
  , logWarnCS
  , toLogStr
  )

import System.IO (stderr)

import GlPlayground.Utils
import UnliftIO.Concurrent (threadDelay)


withLogger ∷ MonadUnliftIO m ⇒ MyLoggerMonad m () → m ()
withLogger m = do
  messageBus ← newEmptyMVar
  let terminateLogger = void ∘ async $ putMVar messageBus Nothing
  let runM = runReaderT (runMyLoggerMonad m) messageBus

  loggerThread ← async $ logger messageBus
  mThread ← asyncBound $ runM `finally` terminateLogger

  wait loggerThread -- Let the logger read everything first
  wait mThread


-- | Log messaging handling routine
--
-- Run in a separate thread.
logger ∷ ∀ m. MonadUnliftIO m ⇒ MessageBus → m ()
logger messageBus = liftIO ∘ fix $ \again →
  takeMVar messageBus >>= \case
    Nothing → logMsg LevelInfo Nothing "Terminating logger…"
    Just (location, _logSource, logLevel, logStr) → do
      logMsg logLevel (Just location) (decodeUtf8 ∘ fromLogStr $ logStr)
      again
  where
    infoFn = T.putStrLn
    errorFn = T.hPutStrLn stderr

    logMsg ∷ LogLevel → Maybe Loc → Text → IO ()
    logMsg logLevel location msg =
      logFn $ mconcat [ "[", level, fromString locationStr, "] ", msg]
      where
        locationStr = case location of
          Nothing → ""
          Just Loc { loc_module = m, loc_start = (line, _) } →
            mconcat [" | ", m, ":", show line]

        (logFn, level) = case logLevel of
          LevelDebug   → (infoFn, "DEBUG")
          LevelInfo    → (infoFn, "INFO")
          LevelWarn    → (errorFn, "WARNING")
          LevelError   → (errorFn, "ERROR")
          LevelOther s → (errorFn, s)


logInfo ∷ (HasCallStack, MonadLogger m, MonadUnliftIO m) ⇒ Text → m ()
logInfo = withFrozenCallStack (void ∘ async ∘ logInfoCS callStack)

logError ∷ (HasCallStack, MonadLogger m, MonadUnliftIO m) ⇒ Text → m ()
logError = withFrozenCallStack (void ∘ async ∘ logErrorCS callStack)

logWarning ∷ (HasCallStack, MonadLogger m, MonadUnliftIO m) ⇒ Text → m ()
logWarning = withFrozenCallStack (void ∘ async ∘ logWarnCS callStack)


type MessageBus = MVar (Maybe LogLine)

newtype MyLoggerMonad m a
  = MyLoggerMonad { runMyLoggerMonad ∷ ReaderT MessageBus m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadUnliftIO
    , MonadReader MessageBus
    )


instance MonadIO m ⇒ MonadLogger (MyLoggerMonad m) where
  monadLoggerLog location logSource logLevel logStr = do
    messageBus ← ask
    liftIO ∘ putMVar messageBus $
      Just (location, logSource, logLevel, toLogStr logStr)


-- * Utils

loggedFail
  ∷ (HasCallStack, MonadFail m, MonadLogger m, MonadUnliftIO m)
  ⇒ String
  → m a
loggedFail msg = withFrozenCallStack $ do
  logError ∘ fromString $ msg
  fail msg
