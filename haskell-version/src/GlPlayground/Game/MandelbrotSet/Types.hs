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


data SubStatic (d ∷ Dimensions) (verticesCount ∷ Nat)
  = SubStatic
  { subStatic'VertexBuffer ∷ KnownVertexBuffer d verticesCount

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
