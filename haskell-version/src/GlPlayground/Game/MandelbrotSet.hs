{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Game.MandelbrotSet
     ( game
     ) where

import Control.Monad (when)

import UnliftIO (MonadUnliftIO, MonadIO (liftIO))
import UnliftIO.Foreign (nullPtr, plusPtr)

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Boilerplate.Shaders
import GlPlayground.Game.MandelbrotSet.ShaderProgram
import GlPlayground.Game.MandelbrotSet.Types

import GlPlayground.Game.Types
import GlPlayground.Logger
import GlPlayground.TypeLevel
import GlPlayground.Types
import GlPlayground.Utils


game
  ∷
  ( MonadUnliftIO m
  , MonadLogger m
  , MonadFail m
  , c ~ VerticesCount D TriangleVertices
  )
  ⇒ Game m (SubStatic D c) (SubState D)
game
  = Game
  { game'Initialize = initialize
  , game'EventHandler = eventHandler
  , game'Update = update
  , game'Render = render
  }


-- * Static data

type D = 'D2 ∷ Dimensions

type VertexType = GL.GLfloat


type TriangleVertices =
  [ P (0 . 0), P (0 . 0) -- FIXME: Test shift of first two items

  , N (1 . 0), N (1 . 0)
  , N (1 . 0), P (1 . 0)
  , P (1 . 0), P (1 . 0)

  , P (1 . 0), P (1 . 0)
  , P (1 . 0), N (1 . 0)
  , N (1 . 0), N (1 . 0)
  ]
  ∷ [Signed FloatingPoint]


-- * Initialization

initialize
  ∷ ∀ m vertices c .
  ( MonadUnliftIO m
  , MonadLogger m
  , MonadFail m
  , vertices ~ TriangleVertices
  , c ~ VerticesCount D vertices
  )
  ⇒ WindowContextEvidence
  → m (SubStatic D c, SubState D)
initialize wndCtxEvidence = do
  logInfo "Playing Mandelbrot set…"

  program ← shaderProgram wndCtxEvidence
  vertexBuffer ← mkKnownVertexBuffer (Proxy @'(D, VertexType, vertices))

  positionAttrLoc ← do
    loc ← getAttribLocation program

    -- TODO vertex buffer evidence
    GL.vertexAttribArray (unTypedAttribLocation loc) GL.$=! GL.Enabled
    GL.vertexAttribPointer (unTypedAttribLocation loc) GL.$=!
      ( GL.ToFloat
      , GL.VertexArrayDescriptor
          (dimensionsToNum ∘ descend $ Proxy @D)
          GL.Float
          0
          (nullPtr `plusPtr` 8) -- FIXME: Test shift of first two items
      )

    pure loc

  wwLoc ← getUniformLocation program
  whLoc ← getUniformLocation program
  timeLoc ← getUniformLocation program

  GL.currentProgram GL.$=! Just program

  pure
    ( SubStatic
    { subStatic'VertexBuffer = vertexBuffer
    , subStatic'Program = program
    , subStatic'PositionAttrLoc = positionAttrLoc
    , subStatic'WindowWidthLoc = wwLoc
    , subStatic'WindowHeightLoc = whLoc
    , subStatic'TimeLoc = timeLoc
    }
    , SubState
    )


-- * Handling events

eventHandler ∷ MonadIO m ⇒ Static (SubStatic D c) → Event → m ()
eventHandler _static _event = pure ()


-- * Updating state

update
  ∷ MonadIO m
  ⇒ Static (SubStatic D c)
  → State (SubState D)
  → m (Maybe (SubState D))
update Static{static'Sub=SubStatic{..}} State{..} = do
  when (fst state'NewCanvasSize ≠ fst state'OldCanvasSize) $
    setUniformValue subStatic'WindowWidthLoc
      (fromIntegral $ fst state'NewCanvasSize)
  when (snd state'NewCanvasSize ≠ snd state'OldCanvasSize) $
    setUniformValue subStatic'WindowHeightLoc
      (fromIntegral $ snd state'NewCanvasSize)

  setUniformValue subStatic'TimeLoc state'NewTime

  pure Nothing


-- * Rendering

render
  ∷ ∀ m c . (MonadIO m, DescendibleAs c Integer)
  ⇒ Static (SubStatic D c)
  → State (SubState D)
  → m ()
render Static{static'Sub=SubStatic{}} State{} = liftIO $ do
  -- GL.bindBuffer GL.ArrayBuffer GL.$=!
  --   Just (vertexBuffer'BufferObject subStatic'VertexBuffer)

  -- GL.vertexAttribArray subStatic'PositionAttrLoc GL.$=! GL.Enabled

  -- GL.vertexAttribPointer subStatic'PositionAttrLoc GL.$=!
  --   ( GL.ToFloat
  --   , GL.VertexArrayDescriptor
  --       (dimensionsToNum ∘ descend $ Proxy @D)
  --       GL.Float
  --       0
  --       nullPtr
  --   )

  GL.drawArrays GL.Triangles 0 $ fromInteger $ descendAs $ Proxy @c

  -- GL.vertexAttribArray (GL.AttribLocation 0) GL.$=! GL.Disabled
