{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Game.Types
     ( Game (..)
     , Static (..), mkStatic
     , State (..), mkState
     ) where

import UnliftIO (MonadUnliftIO)
import UnliftIO.IORef (IORef, newIORef)

import GlPlayground.Types
import GlPlayground.Utils


data Game m subStatic subState
  = Game
  { game'Initialize ∷ WindowContextEvidence → m (subStatic, subState)
  , game'EventHandler ∷ Static subStatic → Event → m ()
  , game'Update ∷ Static subStatic → State subState → m (Maybe subState)
  , game'Render ∷ Static subStatic → State subState → m ()
  }


data Static subStatic
  = Static
  { static'CanvasSizeRef ∷ IORef (Int, Int)
  , static'Sub ∷ {-# UNPACK #-} !subStatic
  }

mkStatic ∷ MonadUnliftIO m ⇒ subStatic → m (Static subStatic)
mkStatic sub = Static ∘ newIORef (0, 0) ↜ pure sub


data State subState
  = State
  { state'OldCanvasSize ∷ (Int, Int)
  , state'NewCanvasSize ∷ (Int, Int)
  , state'OldTime ∷ Double
  , state'NewTime ∷ Double
  , state'Sub ∷ {-# UNPACK #-} !subState
  }
  deriving stock (Eq, Show, Ord) -- Must be plain data in order to satisfy these

instance HasCanvasSize (State subState) where
  getOldCanvasSize State {..} = state'OldCanvasSize
  getNewCanvasSize State {..} = state'NewCanvasSize

mkState ∷ subState → State subState
mkState sub
  = State
  { state'OldCanvasSize = initialCanvasSize
  , state'NewCanvasSize = initialCanvasSize
  , state'OldTime = initialTime
  , state'NewTime = initialTime
  , state'Sub = sub
  }
  where
    initialCanvasSize = (0, 0)
    initialTime = 0
