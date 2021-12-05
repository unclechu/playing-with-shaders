{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.ImpredicativeTypesTest
     ( test
     ) where

import Test.Tasty
import Test.Tasty.HUnit

import GlPlayground.Utils


test ∷ TestTree
test = testGroup "ImpredicativeTypes (since GHC 9.2.1) test"
  [ testCase "Polymorphic value in a monad" $
      monad @=? Just "test [1,2,3] test (True,False)"
  ]


monad ∷ Maybe String
monad = do
  mkF >>= \(f ∷ ∀a. Show a ⇒ a → String) → do
    a ← pure [1, 2, 3 ∷ Word]
    b ← pure (True, False)
    pure $ f a ⋄ " " ⋄ f b

  where
    mkF ∷ Maybe (∀a. Show a ⇒ a → String)
    mkF = pure $ \x → "test " ⋄ show x
