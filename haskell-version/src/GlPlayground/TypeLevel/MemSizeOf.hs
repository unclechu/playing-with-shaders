{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.MemSizeOf
     ( MemSizeMap
     ) where

import Data.Kind (Type)

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.TypeLevel


type MemSizeMap =
  [ '(GL.GLfloat, 4)
  , '(GL.GLdouble, 8)
  ]
  ∷ [(Type, Nat)]
