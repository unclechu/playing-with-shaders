{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.App
     ( runApp
     ) where

import Data.String (fromString)

import Control.Monad.IO.Class (liftIO)

import UnliftIO.IORef (writeIORef)

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

  renderLoop state window render (pure ())
