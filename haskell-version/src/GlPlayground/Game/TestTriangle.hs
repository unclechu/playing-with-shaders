{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Game.TestTriangle
     ( game
     ) where

import UnliftIO (MonadUnliftIO, MonadIO (liftIO))

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.Logger
import GlPlayground.Game.Types
import GlPlayground.Utils


game ∷ (MonadUnliftIO m, MonadLogger m) ⇒ Game m () ()
game
  = Game
  { game'Initialize = const initialize
  , game'EventHandler = \_static _event → pure ()
  , game'Update = \_static _state → pure Nothing
  , game'Render = const render
  }


initialize ∷ (MonadUnliftIO m, MonadLogger m) ⇒ m ((), ())
initialize = mempty ↤ do
  logInfo "Playing test triangle…"
  GL.clearColor GL.$=! GL.Color4 0 0 0 1 -- Black


render ∷ MonadIO m ⇒ State () → m ()
render State{..} = liftIO $ do
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
  where
    r = fromIntegral w ÷ fromIntegral h ∷ Double
    rw x | r > 1.0 = x ÷ r | otherwise = x ∷ Double
    rh x | r < 1.0 = x × r | otherwise = x ∷ Double

    (w, h) = state'NewCanvasSize
    time = state'NewTime
