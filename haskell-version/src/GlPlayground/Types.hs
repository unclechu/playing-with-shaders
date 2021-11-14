{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Types
     ( Event (..)
     , State (..)
     , mkState
     , HasCanvasSize (..)
     ) where

import Data.IORef (IORef, newIORef, readIORef)

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

instance HasCanvasSize State where
  getPrevCanvasSize State {..} = state'LastCanvasSize
  getNextCanvasSize State {..} = readIORef state'CanvasSizeRef

mkState ∷ IO State
mkState
  = State
  ∘ newIORef initialCanvasSize
  ↜ pure initialCanvasSize
  ↜ pure 0
  where
    initialCanvasSize = (0, 0)


class HasCanvasSize a where
  getPrevCanvasSize ∷ a → (Int, Int)
  getNextCanvasSize ∷ a → IO (Int, Int)
