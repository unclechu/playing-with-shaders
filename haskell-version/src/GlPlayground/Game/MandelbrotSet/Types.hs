{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Game.MandelbrotSet.Types
     ( SubStatic (..)
     , SubState (..)
     ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Types


data SubStatic (d ∷ Dimensions)
  = SubStatic
  { subStatic'VertexBuffer ∷ VertexBuffer d

  , subStatic'Program ∷ GL.Program

  , subStatic'PositionAttrLoc ∷ GL.AttribLocation

  , subStatic'WindowWidthLoc ∷ GL.UniformLocation
  , subStatic'WindowHeightLoc ∷ GL.UniformLocation
  , subStatic'TimeLoc ∷ GL.UniformLocation
  }


data SubState (d ∷ Dimensions)
  = SubState
  {
  }
