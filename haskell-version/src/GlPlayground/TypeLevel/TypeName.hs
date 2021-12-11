{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.TypeName
     ( TypeName
     ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Semigroup


type family TypeName (t ∷ k) ∷ Symbol

type instance TypeName GL.GLint    = "GLint"
type instance TypeName GL.GLuint   = "GLuint"
type instance TypeName GL.GLfloat  = "GLfloat"
type instance TypeName GL.GLdouble = "GLdouble"

type instance TypeName (GL.Vector1 t) = "Vector1 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vector2 t) = "Vector2 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vector3 t) = "Vector3 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vector4 t) = "Vector4 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vertex1 t) = "Vertex1 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vertex2 t) = "Vertex2 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vertex3 t) = "Vertex3 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vertex4 t) = "Vertex4 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Vertex4 t) = "Vertex4 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Color3  t) = "Color3 (" ⋄ TypeName t ⋄ ")"
type instance TypeName (GL.Color4  t) = "Color4 (" ⋄ TypeName t ⋄ ")"
