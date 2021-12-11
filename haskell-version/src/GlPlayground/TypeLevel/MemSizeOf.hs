{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.MemSizeOf
     ( MemSizeMap
     , GetMemSize
     , TOctets (..)
     ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.TypeLevel.Arithmetic
import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Descendible
import GlPlayground.Types


type MemSizeMap =
  [ '(GL.GLfloat, 'TOctets 4)
  , '(GL.GLdouble, 'TOctets 8)
  , '(GL.GLint, 'TOctets 4)
  , '(GL.GLuint, 'TOctets 4)
  ]
  ∷ [(Type, TOctets)]


type family GetMemSize (t ∷ k) ∷ TOctets

type instance GetMemSize GL.GLint    = 'TOctets 4
type instance GetMemSize GL.GLuint   = 'TOctets 4
type instance GetMemSize GL.GLfloat  = 'TOctets 4
type instance GetMemSize GL.GLdouble = 'TOctets 8

type instance GetMemSize (GL.Vector1 t) = GetMemSize t
type instance GetMemSize (GL.Vector2 t) = GetMemSize t × 2
type instance GetMemSize (GL.Vector3 t) = GetMemSize t × 3
type instance GetMemSize (GL.Vector4 t) = GetMemSize t × 4
type instance GetMemSize (GL.Vertex1 t) = GetMemSize (GL.Vector1 t)
type instance GetMemSize (GL.Vertex2 t) = GetMemSize (GL.Vector2 t)
type instance GetMemSize (GL.Vertex3 t) = GetMemSize (GL.Vector3 t)
type instance GetMemSize (GL.Vertex4 t) = GetMemSize (GL.Vector4 t)
type instance GetMemSize (GL.Vertex4 t) = GetMemSize (GL.Vector4 t)
type instance GetMemSize (GL.Color3 t) = GetMemSize (GL.Vector3 t)
type instance GetMemSize (GL.Color4 t) = GetMemSize (GL.Vector4 t)


-- | Type-level equivalent of @Octets@
newtype TOctets = TOctets Natural

instance DescendibleAs (n ∷ Natural) Int ⇒ DescendibleAs ('TOctets n) Octets where
  descendAs Proxy = Octets $ descendAs $ Proxy @n


type instance 'TOctets a × 'TOctets b = 'TOctets (a × b)
type instance 'TOctets a × (b ∷ Natural) = 'TOctets (a × b)
type instance (a ∷ Natural) × 'TOctets b = 'TOctets (a × b)
