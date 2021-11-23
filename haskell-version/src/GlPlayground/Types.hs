{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Types
     ( Event (..)
     , Static (..), mkStatic
     , State (..), mkState
     , HasCanvasSize (..)
     , Game (..)

     -- * Additional types
     , Dimensions (..), dimensionsToNum, numToDimensions
     , Dimensional (..)
     , VertexBuffer (..)

     -- * Type level
     , Descendible (..)

     -- * Shaders
     , TypedShader (..)

     -- * Utils
     , Evidence (..)
     , Attest (..)
     , WindowContextEvidence
     ) where

import Data.List (find)
import Data.Proxy (Proxy (Proxy))

import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (IORef, newIORef)

import qualified Graphics.Rendering.OpenGL.GL as GL
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


data Static subStatic
  = Static
  { static'CanvasSizeRef ∷ IORef (Int, Int)
  , static'Sub ∷ {-# UNPACK #-} !subStatic
  }

mkStatic ∷ MonadUnliftIO m ⇒ subStatic → m (Static subStatic)
mkStatic sub = Static ∘ newIORef (0, 0) ↜ pure sub


-- | Generic application state type
data State subState
  = State
  { state'OldCanvasSize ∷ (Int, Int)
  , state'NewCanvasSize ∷ (Int, Int)
  , state'OldTime ∷ Double
  , state'NewTime ∷ Double
  , state'Sub ∷ {-# UNPACK #-} !subState
  }

instance HasCanvasSize (State subState) where
  getOldCanvasSize State {..} = state'OldCanvasSize
  getNewCanvasSize State {..} = state'NewCanvasSize

mkState ∷ subState → State subState
mkState sub
  = State
  { state'OldCanvasSize = initialCanvasSize
  , state'NewCanvasSize = initialCanvasSize
  , state'OldTime = initialTime
  , state'NewTime = initialTime
  , state'Sub = sub
  }
  where
    initialCanvasSize = (0, 0)
    initialTime = 0


class HasCanvasSize a where
  getOldCanvasSize ∷ a → (Int, Int)
  getNewCanvasSize ∷ a → (Int, Int)


data Game m subStatic subState
  = Game
  { game'Initialize ∷ WindowContextEvidence → m (subStatic, subState)
  , game'EventHandler ∷ Static subStatic → Event → m ()
  , game'Update ∷ Static subStatic → State subState → m (Maybe subState)
  , game'Render ∷ Static subStatic → State subState → m ()
  }


-- * Additional types

data Dimensions = D1 | D2 | D3
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance Descendible 'D1 where descend Proxy = D1
instance Descendible 'D2 where descend Proxy = D2
instance Descendible 'D3 where descend Proxy = D3

dimensionsToNum ∷ Num n ⇒ Dimensions → n
dimensionsToNum = \case D1 → 1; D2 → 2; D3 → 3

numToDimensions ∷ (Num n, Eq n) ⇒ n → Maybe Dimensions
numToDimensions n =
  find (dimensionsToNum • (≡ n)) [minBound .. maxBound ∷ Dimensions]


newtype Dimensional (d ∷ Dimensions) t
  = Dimensional { unDimensional ∷ t }
  deriving stock (Eq, Show)


data VertexBuffer (d ∷ Dimensions)
  = VertexBuffer
  { vertexBuffer'BufferObject ∷ GL.BufferObject
  , vertexBuffer'VerticesCount ∷ Dimensional d Int
  }


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
