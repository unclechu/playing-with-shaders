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
     , Signed (..), type P, type N, AsSigned, ShrinkSigned
     , Reciprocal, Even, Odd
     , type (≡), type (≠), type (≤)

     , module Data.Type.Bool
     , module Data.Type.Equality

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
import Data.Type.Equality
import Numeric.Natural (Natural)

import qualified Graphics.Rendering.OpenGL.GL as GL


-- | Type-level floating-point number
--
-- A way to describe floating point numbers on type-level.
-- “FP” stands for “(F)loating (P)oint”.
--
-- @
-- (descendAs (Proxy @(1 . 2)) ∷ Float)  == 1.2
-- (descendAs (Proxy @(1 . 2)) ∷ Double) == 1.2
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

type family AsFP (a ∷ Type) ∷ Type where
  AsFP (i . d) = i . d

type family AsRational (a ∷ Type) ∷ Type where
  AsRational (n % d) = n % d
  AsRational x = AsRational (ToRational x)


data Signed a = Positive a | Negative a

type P x = 'Positive x
type N x = 'Negative x

type family AsSigned (x ∷ k) ∷ Signed a where
  AsSigned (x ∷ Nat) = P x
  AsSigned (i . r) = P (i . r)
  AsSigned (n % d) = P (n % d)
  AsSigned (P x) = P x
  AsSigned (N x) = N x

type family ShrinkSigned (x ∷ Signed k1) ∷ Signed k2 where
  ShrinkSigned (P (P x)) = ShrinkSigned (P x)
  ShrinkSigned (P (N x)) = ShrinkSigned (N x)
  ShrinkSigned (N (P x)) = ShrinkSigned (N x)
  ShrinkSigned (N (N x)) = ShrinkSigned (P x)
  ShrinkSigned x = x


-- | Named type-level value wrapper
data V (s ∷ Symbol) (a ∷ k)


type family ToRational (a ∷ k1) ∷ k2 where
  ToRational (n % d) = n % d -- Identity

  ToRational (x ∷ Nat) = x % 1

  ToRational (P x) = P (AsRational (ToRational x))
  ToRational (N x) = N (AsRational (ToRational x))

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


type a ≡ b = a == b
type a ≠ b = Not (a == b)


type family Reciprocal (a ∷ k1) ∷ Type where
  Reciprocal (a ∷ Nat) = 1 % a
  Reciprocal (i . r) = Reciprocal (AsRational (i . r))
  Reciprocal (n % d) = d % n


type family Even (a ∷ k) ∷ Bool where
  Even (a ∷ Nat) = (a `Mod` 2) ≡ 0
  Even (P (a ∷ Nat)) = (a `Mod` 2) ≡ 0
  Even (N (a ∷ Nat)) = (a `Mod` 2) ≡ 0


type family Odd (a ∷ k) ∷ Bool where
  Odd a = Not (Even a)


-- * Arithmetic operators
--
-- Polymorphic type-level operators.
--
-- Sometimes you have to annotate kind explicitly to help GHC to infer it.

type family (a ∷ k1) × (b ∷ k2) ∷ k3 where
  (a ∷ Nat) × (b ∷ Nat) = a TL.* b
  (an % ad) × (bn % bd) = (an × bn) % (ad × bd)

  (ai . ar) × b = AsRational (ai . ar) × b
  a × (bi . br) = (bi . br) × a

  (a ∷ Nat) × b = AsRational (ToRational a) × b
  a × (b ∷ Nat) = b × a

  P a × P b = P (a × b)
  N a × N b = P (a × b)
  P a × N b = N (a × b)
  N a × P b = P b × N a

  P a × b = P (a × b)
  N a × b = N (a × b)
  a × P b = P b × a
  a × N b = N b × a


type family (a ∷ k1) ^ (exp ∷ k2) ∷ k3 where
  (a ∷ Nat) ^ (exp ∷ Nat) = a TL.^ exp
  (an % ad) ^ (exp ∷ Nat) = (an ^ exp) % (ad ^ exp)
  (ai . ar) ^ exp = AsRational (ai . ar) ^ exp

  -- Fractional exponent is not supported since it’s not supported by
  -- term-level (^) operator.
  -- a ^ (en % ed) = …
  -- a ^ (ei . er) = a ^ AsRational (ei . er)

  -- Mind that term-level "Rational" throws an exception for negative exponent
  P a ^ P exp = P (a ^ exp)
  N a ^ N exp = If (Even exp) (P (Reciprocal a ^ exp)) (N (Reciprocal a ^ exp))
  P a ^ N exp = P (Reciprocal a ^ exp)
  N a ^ P exp = If (Even exp) (P (a ^ exp)) (N (a ^ exp))

  P a ^ exp = P (a ^ exp)
  N a ^ exp = N a ^ P exp
  a ^ P exp = P (a ^ exp)
  a ^ N exp = P a ^ N exp


type family (a ∷ k1) ÷ (b ∷ k2) ∷ k3 where
  (an % ad) ÷ (bn % bd) = (an × bd) % (ad × bn)

  (ai . ar) ÷ b = AsRational (ai . ar) ÷ b
  a ÷ (bi . br) = a ÷ AsRational (bi . br)

  (a ∷ Nat) ÷ b = AsRational (ToRational a) ÷ b
  a ÷ (b ∷ Nat) = a ÷ AsRational (ToRational b)

  P a ÷ P b = P (a ÷ b)
  P a ÷ N b = N (a ÷ b)
  N a ÷ N b = P (a ÷ b)
  N a ÷ P b = N (a ÷ b)

  P a ÷ b = P a ÷ P b
  N a ÷ b = N a ÷ P b
  a ÷ P b = P a ÷ P b
  a ÷ N b = P a ÷ N b


type family (a ∷ k1) + (b ∷ k2) ∷ k3 where
  (a ∷ Nat) + (b ∷ Nat) = a TL.+ b

  (an % ad) + (bn % bd) = V "lcd" (LCD ad bd) + '(an % ad, bn % bd)

  (V "lcd" (lcd ∷ Nat)) + '(an % ad, bn % bd) =
    ((an × (lcd `Div` ad) ∷ Nat) + (bn × (lcd `Div` bd) ∷ Nat)) % lcd

  (ai . ar) + b = AsRational (ai . ar) + b
  a + (bi . br) = a + AsRational (bi . br)

  (a ∷ Nat) + (bn % bd) = AsRational (ToRational a) + (bn % bd)
  (an % ad) + (b ∷ Nat) = (an % ad) + AsRational (ToRational b)

  P a + P b = P (a + b)
  P a + N b = a - b
  N a + N b = N (a + b)
  N a + P b = b - a

  P a + b = P (a + b)
  N a + b = N a + P b
  a + P b = P (a + b)
  a + N b = P a + N b


type family (a ∷ k1) - (b ∷ k2) ∷ Signed k3 where
  (a ∷ Nat) - (b ∷ Nat) = V "≥" (b ≤ a) - '(a, b)
  V "≥" 'True - '(a, b) = P (a TL.- b)
  V "≥" 'False - '(a, b) = N (b TL.- a)

  (an % d) - (bn % d) = V "≥" (bn ≤ an) - '(an, bn, d)
  V "≥" 'True - '(an, bn, d) = P ((an TL.- bn) % d)
  V "≥" 'False - '(an, bn, d) = N ((bn TL.- an) % d)

  (an % ad) - (bn % bd) = V "lcd" (LCD ad bd) - '(an % ad, bn % bd)
  V "lcd" (lcd ∷ Nat) - '(an % ad, bn % bd) =
    V "lcd" lcd - '( an × (lcd `Div` ad) ∷ Nat
                   , bn × (lcd `Div` bd) ∷ Nat
                   , lcd
                   )
  V "lcd" (lcd ∷ Nat) - '(an, bn, d) = V "≥" (bn ≤ an) - '(an, bn, d)

  (ai . ar) - b = AsRational (ai . ar) - b
  a - (bi . br) = a - AsRational (bi . br)

  (a ∷ Nat) - (bn % bd) = AsRational (ToRational a) - (bn % bd)
  (an % ad) - (b ∷ Nat) = (an % ad) - AsRational (ToRational b)

  P a - P b = a - b
  P a - N b = P (a + b)
  N a - N b = b - a
  N a - P b = N (a + b)

  P a - b = P a - P b
  N a - b = N a - P b
  a - P b = P a - P b
  a - N b = P a - N b


type family (a ∷ k1) ≤ (b ∷ k2) ∷ Bool where
  (a ∷ Nat) ≤ (b ∷ Nat) = a TL.<=? b

  (an % ad) ≤ (bn % bd) = V "lcd" (LCD ad bd) ≤ '(an % ad, bn % bd)

  (V "lcd" (lcd ∷ Nat)) ≤ '(an % ad, bn % bd) =
    (an × (lcd `Div` ad) ∷ Nat) ≤ (bn × (lcd `Div` bd) ∷ Nat)

  P a ≤ P b = a ≤ b
  P a ≤ N b = 'False
  N a ≤ P b = 'True
  N a ≤ N b = b ≤ a

  P a ≤ b = a ≤ b
  N a ≤ b = N a ≤ P b
  a ≤ P b = a ≤ b
  a ≤ N b = P a ≤ N b


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


instance KnownNat n ⇒ DescendibleAs n Rational where
  descendAs Proxy = natVal (Proxy @n) % 1

instance KnownNat n ⇒ DescendibleAs n Integer where
  descendAs Proxy = natVal $ Proxy @n

instance KnownNat n ⇒ DescendibleAs n Natural where
  descendAs Proxy = TN.natVal $ Proxy @n

instance KnownSymbol s ⇒ DescendibleAs s String where
  descendAs Proxy = symbolVal $ Proxy @s


instance DescendibleAs a as ⇒ DescendibleAs (P a) as where
  descendAs Proxy = descendAs $ Proxy @a

instance (Num as, DescendibleAs a as) ⇒ DescendibleAs (N a) as where
  descendAs Proxy = negate $ descendAs $ Proxy @a
