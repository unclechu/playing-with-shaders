{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Types
     ( Event (..)
     , State (..)
     , mkState
     , HasCanvasSize (..)
     ) where

import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (IORef, newIORef, readIORef)

import qualified Graphics.UI.GLFW as GLFW

import GlPlayground.Utils


data Event
  = Event'Key GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
  | Event'MousePos Double Double
  | Event'MouseScroll Double Double
  | Event'MouseButton GLFW.MouseButton GLFW.MouseButtonState GLFW.ModifierKeys
  | Event'CanvasResize Int Int
  deriving stock (Eq, Show)


data State
  = State
  { state'CanvasSizeRef ∷ IORef (Int, Int)
  , state'LastCanvasSize ∷ (Int, Int)
  , state'LastTime ∷ Double
  }

instance MonadUnliftIO m ⇒ HasCanvasSize m State where
  getPrevCanvasSize State {..} = pure state'LastCanvasSize
  getNextCanvasSize State {..} = readIORef state'CanvasSizeRef

mkState ∷ MonadUnliftIO m ⇒ m State
mkState
  = State
  ∘ newIORef initialCanvasSize
  ↜ pure initialCanvasSize
  ↜ pure 0
  where
    initialCanvasSize = (0, 0)


class HasCanvasSize m a where
  getPrevCanvasSize ∷ a → m (Int, Int)
  getNextCanvasSize ∷ a → m (Int, Int)
