{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.App
     ( runApp
     ) where

import Control.Monad (when)

import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.Foreign (withArrayLen, nullPtr)
import UnliftIO.IORef (writeIORef, readIORef)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders as GLSL
import qualified Graphics.UI.GLFW as GLFW

import GlPlayground.Boilerplate
import GlPlayground.Logger
import GlPlayground.Render
import GlPlayground.Shaders
import GlPlayground.Types
import GlPlayground.Utils


runApp ∷ IO ()
runApp = withLogger $ do
  state@State{..} ← mkState ()

  window ← mkWindow $ \case
    Event'CanvasResize w h →
      writeIORef state'CanvasSizeRef (w, h)

    _ → pure ()

  program ← mandelbrotSetShaderProgram (evidence window)
  attribLocation ← liftIO ∘ GL.get $ GL.attribLocation program "position"

  let
    triangleVertexes ∷ [GL.GLfloat]
    triangleVertexes =
      [ -1, -1
      , -1, 1
      , 1, 1

      , 1, 1
      , 1, -1
      , -1, -1
      ]
      -- [ -1, -1, 1
      -- , -1 , 1, 1
      -- , 1, 1, 1

      -- , 1, 1, 1
      -- , 1, -1, 1
      -- , -1, -1, 1
      -- ]

    createVertexBuffer
      ∷ MonadUnliftIO m
      ⇒ [GL.GLfloat]
      → m (GL.BufferObject, Int)
    createVertexBuffer vertexes = do
      bufferObject ← liftIO GL.genObjectName
      liftIO $ GL.bindBuffer GL.ArrayBuffer GL.$=! Just bufferObject

      vertexesCount ← withArrayLen vertexes $ \count arr → do
        liftIO $ GL.bufferData GL.ArrayBuffer GL.$=!
          (fromIntegral count × 4, arr, GL.StaticDraw)
        pure (count `div` dimensions)

      GL.vertexAttribArray attribLocation GL.$=! GL.Enabled

      GL.vertexAttribPointer attribLocation GL.$=!
        ( GL.ToFloat
        , GL.VertexArrayDescriptor (fromIntegral dimensions) GL.Float 0 nullPtr
        )

      pure (bufferObject, vertexesCount)

      where
        dimensions = 2 ∷ Int

  (vertexBuffer, vertexesCount) ← createVertexBuffer triangleVertexes
  GL.bindBuffer GL.ArrayBuffer GL.$=! Just vertexBuffer
  GL.clearColor GL.$=! GL.Color4 0 0 0 1 -- Black

  locations ← liftIO $ (,,)
    ∘ GLSL.uniformLocation program "ww"
    ↜ GLSL.uniformLocation program "wh"
    ↜ GLSL.uniformLocation program "time"

  mainLoop
    window
    state
    (update program locations)
    (render (vertexBuffer, vertexesCount))
    (pure ())


update
  ∷ (MonadUnliftIO m, MonadFail m, MonadLogger m)
  ⇒ GLSL.Program
  → (GL.UniformLocation, GL.UniformLocation, GL.UniformLocation)
  → (State subState)
  → m (State subState)
update program (wwVarLoc, whVarLoc, timeVarLoc) state@State{..} = do
  canvasSize ← readIORef state'CanvasSizeRef

  time ←
    liftIO GLFW.getTime >>=
      maybe (loggedFail "Failed to read current time!") pure

  liftIO $ GLSL.currentProgram GL.$=! Just program

  when (fst canvasSize ≠ fst state'NewCanvasSize) $
    liftIO $ GLSL.uniform wwVarLoc GL.$=!
      (fromIntegral $ fst canvasSize ∷ GL.GLint)
  when (snd canvasSize ≠ snd state'NewCanvasSize) $
    liftIO $ GLSL.uniform whVarLoc GL.$=!
      (fromIntegral $ snd canvasSize ∷ GL.GLint)

  liftIO $ GLSL.uniform timeVarLoc GL.$=! (time ∷ GL.GLdouble)

  pure state
    { state'OldCanvasSize = state'NewCanvasSize
    , state'NewCanvasSize = canvasSize
    , state'OldTime = state'NewTime
    , state'NewTime = time
    }
