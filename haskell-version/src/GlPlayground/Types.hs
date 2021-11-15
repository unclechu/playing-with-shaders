{-# LANGUAGE DerivingStrategies #-}
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
  , state'OldCanvasSize ∷ (Int, Int)
  , state'NewCanvasSize ∷ (Int, Int)
  , state'OldTime ∷ Double
  , state'NewTime ∷ Double
  }

instance HasCanvasSize State where
  getOldCanvasSize State {..} = state'OldCanvasSize
  getNewCanvasSize State {..} = state'NewCanvasSize

mkState ∷ MonadUnliftIO m ⇒ m State
mkState
  = State
  ∘ newIORef initialCanvasSize
  ↜ pure initialCanvasSize
  ↜ pure initialCanvasSize
  ↜ pure initialTime
  ↜ pure initialTime
  where
    initialCanvasSize = (0, 0)
    initialTime = 0


class HasCanvasSize a where
  getOldCanvasSize ∷ a → (Int, Int)
  getNewCanvasSize ∷ a → (Int, Int)
