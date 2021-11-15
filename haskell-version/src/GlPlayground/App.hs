{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.App
     ( runApp
     ) where

import Data.String (fromString)

import Control.Monad.IO.Class (liftIO)

import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (writeIORef, readIORef)

import qualified Graphics.UI.GLFW as GLFW

import GlPlayground.Boilerplate
import GlPlayground.Logger
import GlPlayground.Render
import GlPlayground.Types
import GlPlayground.Utils


runApp ∷ IO ()
runApp = withLogger $ do
  state@State{..} ← mkState

  window ← mkWindow $ \case
    Event'CanvasResize w h →
      writeIORef state'CanvasSizeRef (w, h)

    _ → pure ()

    -- x → logInfo ∘ fromString ∘ show $ x

  mainLoop window state update render (pure ())


update ∷ (MonadUnliftIO m, MonadFail m) ⇒ State → m State
update state@State{..} = do
  canvasSize ← readIORef state'CanvasSizeRef

  time ←
    liftIO GLFW.getTime >>=
      maybe (fail "Failed to read current time!") pure

  pure state
    { state'OldCanvasSize = state'NewCanvasSize
    , state'NewCanvasSize = canvasSize
    , state'OldTime = state'NewTime
    , state'NewTime = time
    }
