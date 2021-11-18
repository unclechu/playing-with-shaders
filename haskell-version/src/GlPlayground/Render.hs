{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Render
     ( render
     ) where

import UnliftIO (MonadIO (liftIO))
import UnliftIO.Foreign (nullPtr)

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Types
import GlPlayground.Utils


render ∷ MonadIO m ⇒ (GL.BufferObject, Int) → State subState → m ()
render (vertexBuffer, vertexesCount) State{..} = liftIO $ do
  -- GL.bindBuffer GL.ArrayBuffer GL.$=! Just vertexBuffer

  -- GL.vertexAttribArray (GL.AttribLocation 0) GL.$=! GL.Enabled

  -- GL.vertexAttribPointer (GL.AttribLocation 0) GL.$=!
  --   ( GL.ToFloat
  --   , GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr
  --   )

  GL.drawArrays GL.Triangles 0 (fromIntegral vertexesCount)

  -- GL.vertexAttribArray (GL.AttribLocation 0) GL.$=! GL.Disabled

  where
    r = fromIntegral w ÷ fromIntegral h ∷ Double
    rw x | r > 1.0 = x ÷ r | otherwise = x ∷ Double
    rh x | r < 1.0 = x × r | otherwise = x ∷ Double

    (w, h) = state'NewCanvasSize
    time = state'NewTime

    _testTriangle =
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
