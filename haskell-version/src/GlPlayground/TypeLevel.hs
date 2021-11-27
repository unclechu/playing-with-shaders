{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Some generic type-level helpers
module GlPlayground.TypeLevel
     ( FP, fpFloatVal, fpDoubleVal, type (.)

     -- * Descent
     , Descendible (..)
     , DescendibleAs (..)
     ) where

import GHC.Float (rationalToFloat, rationalToDouble)
import GHC.TypeLits

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))


-- | Type-level floating-point number
--
-- A way to describe floating point numbers on type-level.
-- “FP” stands for “(F)loating (P)oint”.
--
-- @
-- frFloatVal (Proxy @(Fr 1 2)) == 1.2
-- frDoubleVal (Proxy @(Fr 1 2)) == 1.2
-- @
data FP (integer ∷ Nat) (remainder ∷ Nat)

-- | Type-level rational value
data TRational (numerator ∷ Nat) (denominator ∷ Nat)

fpFloatVal
  ∷ ∀ n d i r .
  ( TRational n d ~ FpToRational (FP i r)
  , KnownNat n
  , KnownNat d
  )
  ⇒ Proxy (FP i r)
  → Float
fpFloatVal Proxy = rationalToFloat (natVal $ Proxy @n) (natVal $ Proxy @d)

fpDoubleVal
  ∷ ∀ n d i r .
  ( TRational n d ~ FpToRational (FP i r)
  , KnownNat n
  , KnownNat d
  )
  ⇒ Proxy (FP i r)
  → Double
fpDoubleVal Proxy = rationalToDouble (natVal $ Proxy @n) (natVal $ Proxy @d)

-- | A hack to write floating point numbers in type-level like in term-level
type family (.) a b ∷ Type where
  a.b = FP a b

type family FpToRational (a ∷ k1) ∷ k2 where
  FpToRational (FP i r) = FpToRational '(i, 1, r)

  FpToRational '(numerator, denominator, 0) = TRational numerator denominator
  FpToRational '(numerator, denominator, remainder) =
    TRational
      (numerator * (10 ^ FpToRational '(0, remainder)) + remainder)
      (denominator * (10 ^ FpToRational '(0, remainder)))

  -- Calculate exponent
  FpToRational '(exponent, 0) = exponent
  FpToRational '(exponent, x) = FpToRational '(exponent + 1, x `Div` 10)


-- * Descent

class Descendible (a ∷ k) where
  descend ∷ Proxy a → k

class DescendibleAs (a ∷ k) (as ∷ Type) where
  descendAs ∷ Proxy a → as
