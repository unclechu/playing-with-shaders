{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.App
     ( runApp
     ) where

import Data.IORef (writeIORef)
import Data.String (fromString)

import GlPlayground.Boilerplate
import GlPlayground.Render
import GlPlayground.Types
import GlPlayground.Utils


runApp ∷ IO ()
runApp = do
  state@State{..} ← mkState

  window ← mkWindow $ \case
    Event'CanvasResize w h →
      writeIORef state'CanvasSizeRef (w, h)

    _ → pure ()

    -- x → logInfo ∘ fromString ∘ show $ x

  renderLoop state window render (pure ())
