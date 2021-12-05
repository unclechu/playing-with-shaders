{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.App
     ( runApp
     ) where


import GlPlayground.Logger (withLogger)
import GlPlayground.Game (playGame)
import qualified GlPlayground.Game.MandelbrotSet as MandelbrotSet
import qualified GlPlayground.Game.TestTriangle as TestTriangle


runApp ∷ IO ()
runApp = withLogger $ playGame MandelbrotSet.game
