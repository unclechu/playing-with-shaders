{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Game.MandelbrotSet.Types
     ( SubStatic (..)
     , SubState (..)
     ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.TypeLevel
import GlPlayground.Types


data SubStatic (d ∷ Dimensions) (verticesCount ∷ Natural)
  = SubStatic
  { subStatic'VertexBuffer ∷ KnownVertexBuffer d verticesCount

  , subStatic'Program ∷ GL.Program

  , subStatic'PositionAttrLoc ∷ TypedAttribLocation "position" GL.GLfloat

  , subStatic'WindowWidthLoc ∷ TypedUniformLocation "ww" GL.GLint
  , subStatic'WindowHeightLoc ∷ TypedUniformLocation "wh" GL.GLint
  , subStatic'TimeLoc ∷ TypedUniformLocation "time" GL.GLdouble
  }


data SubState (d ∷ Dimensions)
  = SubState
  {
  }
