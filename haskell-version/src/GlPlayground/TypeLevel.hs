{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
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
     ( FP, TRational, type (.), type (%), AsFP, AsRational, ToRational
     , Nat, KnownNat, natVal, Div, Mod
     , Symbol, KnownSymbol, symbolVal
     , module Data.Type.Bool

     -- * Arithmetic operators
     , type (×), type (^), type (÷), type (+), type (-)

     -- * Descent
     , Descendible (..)
     , DescendibleAs (..)
     ) where

import GHC.Float (rationalToFloat, rationalToDouble)
import qualified GHC.TypeLits as TL
import qualified GHC.TypeNats as TN

import GHC.TypeLits
  ( Nat, KnownNat, natVal, Div, Mod
  , Symbol, KnownSymbol, symbolVal
  )

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Ratio ((%))
import Data.Type.Bool
import Numeric.Natural (Natural)

import qualified Graphics.Rendering.OpenGL.GL as GL


-- | Type-level floating-point number
--
-- A way to describe floating point numbers on type-level.
-- “FP” stands for “(F)loating (P)oint”.
--
-- @
-- frFloatVal (Proxy @(Fr 1 2)) == 1.2
-- frDoubleVal (Proxy @(Fr 1 2)) == 1.2
-- @
--
-- WARNING! Mind that you can’t write down such number as @1.01@
-- (remainder with leading zeroes). For this cases use "TRational".
-- You can do something like @AsRational (1 + (1 % 100))@.
data FP (integer ∷ Nat) (remainder ∷ Nat)

instance
  ( n % d ~ AsRational (i . r)
  , KnownNat n
  , KnownNat d
  ) ⇒ DescendibleAs (i . r) Float
  where
  descendAs Proxy = rationalToFloat (natVal $ Proxy @n) (natVal $ Proxy @d)

instance
  ( n % d ~ AsRational (i . r)
  , KnownNat n
  , KnownNat d
  ) ⇒ DescendibleAs (i . r) Double
  where
  descendAs Proxy = rationalToDouble (natVal $ Proxy @n) (natVal $ Proxy @d)

instance
  ( n % d ~ AsRational (i . r)
  , KnownNat n
  , KnownNat d
  ) ⇒ DescendibleAs (i . r) Rational
  where
  descendAs Proxy = natVal (Proxy @n) % natVal (Proxy @d)


-- | Type-level rational value
data TRational (numerator ∷ Nat) (denominator ∷ Nat)

instance (KnownNat n, KnownNat d) ⇒ DescendibleAs (n % d) Rational where
  descendAs Proxy = natVal (Proxy @n) % natVal (Proxy @d)

instance (KnownNat n, KnownNat d) ⇒ DescendibleAs (n % d) Float where
  descendAs Proxy = rationalToFloat (natVal $ Proxy @n) (natVal $ Proxy @d)

instance (KnownNat n, KnownNat d) ⇒ DescendibleAs (n % d) Double where
  descendAs Proxy = rationalToDouble (natVal $ Proxy @n) (natVal $ Proxy @d)


-- | A hack to write floating point numbers in type-level like in term-level
type i . r = FP i r
type n % d = TRational n d

type family AsFP (a ∷ Type) ∷ Type where AsFP (i . d) = i . d

type family AsRational (a ∷ Type) ∷ Type where
  AsRational (n % d) = n % d
  AsRational x = AsRational (ToRational x)


-- | Named type-level value wrapper
data V (s ∷ Symbol) (a ∷ k)


type family ToRational (a ∷ k1) ∷ k2 where
  ToRational (n % d) = n % d -- Identity
  ToRational (x ∷ Nat) = x % 1
  ToRational (i . r) = AsRational (ToRational (V "Transform" '(i, 1, r)))

  ToRational (V "Transform" '(numerator, denominator, 0)) =
    numerator % denominator
  ToRational (V "Transform" '(numerator, denominator, remainder)) =
    AsRational
      (ToRational
        (V "WithExponent"
          '( ToRational (V "GetExponent" '(0, remainder)) ∷ Nat
           , numerator ∷ Nat
           , denominator ∷ Nat
           , remainder ∷ Nat
           )))

  ToRational
    (V "WithExponent"
      '(exponent ∷ Nat, numerator ∷ Nat, denominator ∷ Nat, remainder ∷ Nat))
    = ((numerator × (10 ^ exponent ∷ Nat) ∷ Nat) + remainder ∷ Nat)
    % (denominator × (10 ^ exponent ∷ Nat) ∷ Nat)

  ToRational (V "GetExponent" '(exponent ∷ Nat, 0)) = exponent ∷ Nat
  ToRational (V "GetExponent" '(exponent ∷ Nat, n ∷ Nat)) =
    ToRational (V "GetExponent" '(exponent + 1 ∷ Nat, n `Div` 10)) ∷ Nat


-- | Find least common denominator
type family LCD (a ∷ k1) (b ∷ k2) ∷ Nat where
  LCD (a ∷ Nat) a = a
  LCD (a ∷ Nat) (b ∷ Nat) = LCD 1 '(a, b)

  LCD (step ∷ Nat) '(a ∷ Nat, b ∷ Nat) =
    LCD step ('Left '((a × step) `Mod` b, a, b))

  LCD (step ∷ Nat) ('Left '(0, a ∷ Nat, b ∷ Nat)) =
    a × step ∷ Nat

  LCD (step ∷ Nat) ('Left '(_ ∷ Nat, a ∷ Nat, b ∷ Nat)) =
    LCD step ('Right '((b × step) `Mod` a, a, b))

  LCD (step ∷ Nat) ('Right '(0, a ∷ Nat, b ∷ Nat)) =
    b × step ∷ Nat

  LCD (step ∷ Nat) ('Right '(_ ∷ Nat, a ∷ Nat, b ∷ Nat)) =
    LCD (step + 1 ∷ Nat) '(a, b)


-- * Arithmetic operators
--
-- Polymorphic type-level operators.
--
-- Sometimes you have to annotate kind explicitly to help GHC to infer it.

type family (a ∷ k1) × (b ∷ k2) ∷ k3 where
  (a ∷ Nat) × (b ∷ Nat) = a TL.* b
  (an % ad) × (bn % bd) = (an × bn) % (ad × bd)

  (ai . ar) × b = AsRational (ai . ar) × b
  a × (bi . br) = a × AsRational (bi . br)

  (a ∷ Nat) × (bn % bd) = AsRational (ToRational a) × (bn % bd)
  (an % ad) × (b ∷ Nat) = (an % ad) × AsRational (ToRational b)


type family (a ∷ k1) ^ (b ∷ k2) ∷ k3 where
  (a ∷ Nat) ^ (b ∷ Nat) = a TL.^ b
  (an % ad) ^ (b ∷ Nat) = (an ^ b) % (ad ^ b)
  (ai . ar) ^ b = AsRational (ai . ar) ^ b


type family (a ∷ k1) ÷ (b ∷ k2) ∷ k3 where
  (an % ad) ÷ (bn % bd) = (an × bd) % (ad × bn)

  (ai . ar) ÷ b = AsRational (ai . ar) ÷ b
  a ÷ (bi . br) = a ÷ AsRational (bi . br)

  (a ∷ Nat) ÷ b = AsRational (ToRational a) ÷ b
  a ÷ (b ∷ Nat) = a ÷ AsRational (ToRational b)


type family (a ∷ k1) + (b ∷ k2) ∷ k3 where
  (a ∷ Nat) + (b ∷ Nat) = a TL.+ b

  (an % ad) + (bn % bd) = V "lcd" (LCD ad bd) + '(an % ad, bn % bd)

  (V "lcd" (lcd ∷ Nat)) + '(an % ad, bn % bd) =
    ((an × (lcd `Div` ad) ∷ Nat) + (bn × (lcd `Div` bd) ∷ Nat)) % lcd

  (ai . ar) + b = AsRational (ai . ar) + b
  a + (bi . br) = a + AsRational (bi . br)

  (a ∷ Nat) + (bn % bd) = AsRational (ToRational a) + (bn % bd)
  (an % ad) + (b ∷ Nat) = (an % ad) + AsRational (ToRational b)


type family (a ∷ k1) - (b ∷ k2) ∷ k3 where
  (a ∷ Nat) - (b ∷ Nat) = a TL.- b

  (an % ad) - (bn % bd) = V "lcd" (LCD ad bd) - '(an % ad, bn % bd)

  (V "lcd" (lcd ∷ Nat)) - '(an % ad, bn % bd) =
    ((an × (lcd `Div` ad) ∷ Nat) - (bn × (lcd `Div` bd) ∷ Nat)) % lcd

  (ai . ar) - b = AsRational (ai . ar) - b
  a - (bi . br) = a - AsRational (bi . br)

  (a ∷ Nat) - (bn % bd) = AsRational (ToRational a) - (bn % bd)
  (an % ad) - (b ∷ Nat) = (an % ad) - AsRational (ToRational b)


-- * Descent

class Descendible (a ∷ k) where
  descend ∷ Proxy a → k


class DescendibleAs (a ∷ k) (as ∷ Type) where
  descendAs ∷ Proxy a → as

  default descendAs :: (Descendible a, k ~ as) => Proxy a -> as
  descendAs = descend


instance Descendible 'GL.VertexShader where descend Proxy = GL.VertexShader
instance DescendibleAs 'GL.VertexShader GL.ShaderType

instance Descendible 'GL.FragmentShader where descend Proxy = GL.FragmentShader
instance DescendibleAs 'GL.FragmentShader GL.ShaderType


instance KnownNat n ⇒ DescendibleAs n Integer where
  descendAs Proxy = natVal $ Proxy @n

instance KnownNat n ⇒ DescendibleAs n Natural where
  descendAs Proxy = TN.natVal $ Proxy @n

instance KnownSymbol s ⇒ DescendibleAs s String where
  descendAs Proxy = symbolVal $ Proxy @s
