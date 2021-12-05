{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Test.Tasty

import GlPlayground.TypeLevel (test)


main ∷ IO ()
main = defaultMain tests


tests ∷ TestTree
tests = testGroup "GL Playground tests"
  [ GlPlayground.TypeLevel.test
  ]
