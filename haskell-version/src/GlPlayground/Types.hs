{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Types
     ( Event (..)
     , HasCanvasSize (..)

     -- * Additional types
     , Dimensions (..), dimensionsToNum, numToDimensions
     , Dimensional (..)
     , VertexBuffer (..)
     , KnownVertexBuffer (..)
     , Octets (..)

     -- * Shaders
     , TypedShader (..)

     -- * Utils
     , Evidence (..)
     , Attest (..)
     , WindowContextEvidence
     ) where

import GHC.TypeLits

import Data.Kind (Type)
import Data.List (find)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Bool (If)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import GlPlayground.TypeLevel
import GlPlayground.Utils


data Event
  = Event'Key GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
  | Event'MousePos Double Double
  | Event'MouseScroll Double Double
  | Event'MouseButton GLFW.MouseButton GLFW.MouseButtonState GLFW.ModifierKeys
  | Event'CanvasResize Int Int
  deriving stock (Eq, Show)


class HasCanvasSize a where
  getOldCanvasSize ∷ a → (Int, Int)
  getNewCanvasSize ∷ a → (Int, Int)


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
  , vertexBuffer'SizeInOctets ∷ Octets
  }


data KnownVertexBuffer (d ∷ Dimensions) (verticesCount ∷ Nat)
  = KnownVertexBuffer
  { knownVertexBuffer'BufferObject ∷ GL.BufferObject
  , knownVertexBuffer'SizeInOctets ∷ Octets
  }


newtype Octets = Octets { unOctets ∷ Int }
  deriving stock (Eq, Show, Ord)
  deriving newtype (Num, Integral, Enum, Bounded, Real)


-- * Type level


-- * Shaders

newtype TypedShader (t ∷ GL.ShaderType)
  = TypedShader { unTypedShader ∷ GL.Shader }

instance Descendible 'GL.VertexShader where
  descend Proxy = GL.VertexShader

instance Descendible 'GL.FragmentShader where
  descend Proxy = GL.FragmentShader


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
