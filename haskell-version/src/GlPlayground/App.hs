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

import Control.Monad ((<=<), (>=>))

import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.IORef (writeIORef, readIORef)

import qualified Graphics.UI.GLFW as GLFW

import GlPlayground.Boilerplate.Backbone
import GlPlayground.Logger
import GlPlayground.Types
import GlPlayground.Utils
import qualified GlPlayground.Game.MandelbrotSet as MandelbrotSet
import qualified GlPlayground.Game.TestTriangle as TestTriangle


runApp ∷ IO ()
runApp = withLogger $ do
  mkGlApp
    (game'Initialize >=> \(a, b) → (,) ∘ mkStatic a ↜ pure (mkState b))
    (\static@Static{..} ev → do
        case ev of
          Event'CanvasResize w h → writeIORef static'CanvasSizeRef (w, h)
          _ → pure ()

        game'EventHandler static ev
    )
    (\static → gameUpdate static <=< update static)
    (\static → game'Render static)
    (pure ())

  where
    Game{..} = MandelbrotSet.game

    gameUpdate static state =
      game'Update static state
        • maybe state (\x → state { state'Sub = x })


update
  ∷ (MonadUnliftIO m, MonadFail m, MonadLogger m)
  ⇒ Static subStatic
  → State subState
  → m (State subState)
update Static{..} state@State{..} = do
  canvasSize ← readIORef static'CanvasSizeRef

  time ←
    liftIO GLFW.getTime >>=
      maybe (loggedFail "Failed to read current time!") pure

  pure state
    { state'OldCanvasSize = state'NewCanvasSize
    , state'NewCanvasSize = canvasSize
    , state'OldTime = state'NewTime
    , state'NewTime = time
    }
