{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Render
     ( render
     ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Types
import GlPlayground.Utils


render ∷ State → (Int, Int) → Double → IO State
render prevState canvasSize@(w, h) time = do
  GL.renderPrimitive GL.Triangles $ do
    let
      anim = 0.1 × time
      x = 0.5

    GL.color $ GL.Color3 @Double 0.1 0.2 0.3
    GL.vertex $ GL.Vertex3 0 (rh $ x + anim) 0
    GL.color $ GL.Color3 @Double 0.3 0.2 0.1
    GL.vertex $ GL.Vertex3 (rw $ (-x) + (-anim)) (rh $ (-x) + (-anim)) 0
    GL.color $ GL.Color3 @Double 0.3 0.3 0.3
    GL.vertex $ GL.Vertex3 (rw $ x + anim) (rh $ (-x) + (-anim)) 0

  pure nextState

  where
    r = fromIntegral w ÷ fromIntegral h ∷ Double
    rw x | r > 1.0 = x ÷ r | otherwise = x ∷ Double
    rh x | r < 1.0 = x × r | otherwise = x ∷ Double

    nextState = prevState
      { state'LastTime = time
      , state'LastCanvasSize = canvasSize
      }
