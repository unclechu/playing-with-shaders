{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Game
     ( module GlPlayground.Game.Types
     , playGame
     ) where

import Control.Monad ((<=<), (>=>))

import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.IORef (writeIORef, readIORef)

import qualified Graphics.UI.GLFW as GLFW

import GlPlayground.Boilerplate.Backbone (mkGlApp)
import GlPlayground.Game.Types
import GlPlayground.Logger
import GlPlayground.Types
import GlPlayground.Utils


playGame
  ∷ (MonadUnliftIO m, MonadFail m, MonadLogger m)
  ⇒ Game m subStatic subState
  → m ()
playGame Game{..} =
  mkGlApp
    (game'Initialize >=> \(a, b) → (,) ∘ mkStatic a ↜ pure (mkState b))
    (\static@Static{..} ev → do
        case ev of
          Event'CanvasResize w h → writeIORef static'CanvasSizeRef (w, h)
          _ → pure ()

        game'EventHandler static ev
    )
    (\static → gameUpdate static <=< genericUpdate static)
    game'Render
    (pure ())

  where
    gameUpdate static state =
      game'Update static state
        • maybe state (\x → state { state'Sub = x })


genericUpdate
  ∷ (MonadUnliftIO m, MonadFail m, MonadLogger m)
  ⇒ Static subStatic
  → State subState
  → m (State subState)
genericUpdate Static{..} state@State{..} = do
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
