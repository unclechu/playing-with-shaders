{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel
     ( test
     ) where

import Test.Tasty
import Test.Tasty.HUnit

import "gl-playground" GlPlayground.TypeLevel
import qualified GlPlayground.TypeLevel.MemSizeOf (test)


-- TODO add more tests for type-level stuff
test ∷ TestTree
test = testGroup "Type-level"
  [ testArithmeticOperators
  , GlPlayground.TypeLevel.MemSizeOf.test
  ]


-- * Arithmetic operators

testArithmeticOperators ∷ TestTree
testArithmeticOperators = testGroup "Arithmetic operators"
  [ testMultiplicationOperator
  ]


-- TODO add more tests for arithmetic operators
testMultiplicationOperator ∷ TestTree
testMultiplicationOperator = testGroup "Multiplication operator"
  [ test2x2
  , testFloatingPoint
  , testRational
  , testSigned
  ]
  where
    test2x2
      ∷
      ( 2 × 2 ~ 4
      , (2 % 1) × (2 % 1) ~ (4 % 1)
      , (2 . 0) × (2 . 0) ~ (4 % 1)
      )
      ⇒ TestTree
    test2x2 = testCase "2 × 2 = 4" mempty

    testFloatingPoint
      ∷
      ( -- FIXME find lowest denominator
        -- (1 . 2) × (1 . 3) ~ (39 % 25)
        (1 . 2) × (1 . 3) ~ ((39 × (100 `Div` 25)) % 100)
      )
      ⇒ TestTree
    testFloatingPoint = testCase "Floating point" mempty

    testRational
      ∷
      ( -- FIXME find lowest denominator
        -- (3 % 4) × (2 % 3) ~ (1 % 2)
        (3 % 4) × (2 % 3) ~ (6 % 12)
      )
      ⇒ TestTree
    testRational = testCase "TRational" mempty

    testSigned
      ∷
      ( P 3 × P 4 ~ P 12
      , N 3 × N 4 ~ P 12
      , P 3 × N 4 ~ N 12
      , N 3 × P 4 ~ N 12

      -- FIXME find lowest denominator
      -- , P (3 % 4) × P (2 % 3) ~ P (1 % 2)
      -- , N (3 % 4) × N (2 % 3) ~ P (1 % 2)
      -- , P (3 % 4) × N (2 % 3) ~ N (1 % 2)
      -- , N (3 % 4) × P (2 % 3) ~ N (1 % 2)
      , P (3 % 4) × P (2 % 3) ~ P (6 % 12)
      , N (3 % 4) × N (2 % 3) ~ P (6 % 12)
      , P (3 % 4) × N (2 % 3) ~ N (6 % 12)
      , N (3 % 4) × P (2 % 3) ~ N (6 % 12)
      )
      ⇒ TestTree
    testSigned = testCase "Signed" mempty
