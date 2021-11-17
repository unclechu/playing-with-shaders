{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Types
     ( Event (..)
     , State (..)
     , mkState
     , HasCanvasSize (..)

     -- * Type level
     , Descendible (..)

     -- * Shaders
     , TypedShader (..)

     -- * Utils
     , Evidence (..)
     , Attest (..)
     , WindowContextEvidence
     ) where

import Data.Proxy (Proxy (Proxy))

import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (IORef, newIORef)

import qualified Graphics.Rendering.OpenGL.GL.Shaders as GLSL
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


-- * Type level

class Descendible (a ∷ k) where
  descend ∷ Proxy a → k


-- * Shaders

newtype TypedShader (t ∷ GLSL.ShaderType)
  = TypedShader { unTypedShader ∷ GLSL.Shader }

instance Descendible 'GLSL.VertexShader where
  descend Proxy = GLSL.VertexShader

instance Descendible 'GLSL.FragmentShader where
  descend Proxy = GLSL.FragmentShader


-- * Utils

class Evidence source evidence where
  evidence ∷ source → evidence

class Attest evidence where
  attest ∷ evidence → ()


-- The constructor must not be exported
data WindowContextEvidence = WindowContextEvidence

instance Evidence GLFW.Window WindowContextEvidence where
  evidence _window = WindowContextEvidence

instance Attest WindowContextEvidence where
  attest WindowContextEvidence = ()
