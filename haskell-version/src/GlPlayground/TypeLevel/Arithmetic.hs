{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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

-- For some reason I get this error:
--  • Expected kind ‘k’, but ‘x :: Nat’ has kind ‘Nat’
--  • In the first argument of ‘ToSigned’, namely ‘(x :: Nat)’
--    In the type family declaration for ‘ToSigned’
-- {-# LANGUAGE GHC2021 #-}

-- | Type-level arithmetics for fractional and signed numbers
module GlPlayground.TypeLevel.Arithmetic
     ( module GHC.TypeLits

     , FloatingPoint, type (.)
     , TRational, ToTRational, type (%)
     , Signed (..), type P, type N, ToSigned, ToSignedTRational, ShrinkSigned

     , LCD, Reciprocal, Even, Odd

     -- * Arithmetic operators
     , type (×), type (^), type (÷)
     , type (+), type (-)
     , type (≤)
     ) where

import GHC.Float (rationalToFloat, rationalToDouble)
import GHC.Real (Ratio ((:%)))
import GHC.TypeLits (Div, Mod)
import qualified GHC.TypeLits as TL

import Data.Ratio ((%))

import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Descendible


-- | Type-level floating-point number
--
-- A way to describe floating point numbers on type-level.
--
-- @
-- (descendAs (Proxy @(1 . 2)) ∷ Float)  == 1.2
-- (descendAs (Proxy @(1 . 2)) ∷ Double) == 1.2
-- @
--
-- WARNING! Mind that you can’t write down such number as @1.01@
-- (remainder with leading zeroes). For this cases use "TRational".
-- You can do something like @1 + (1 % 100) :: TRational@.
data FloatingPoint = Nat :. Nat


-- | Type-level "Rational"
--
-- @
-- (descendAs (Proxy @(1 % 2)) ∷ Rational) == 1 % 2
-- @
type TRational = Ratio Nat


-- | Type-level rational value
-- data TRational = TRational { numerator ∷ Nat, denominator ∷ Nat }
-- data TRational (numerator ∷ Nat) (denominator ∷ Nat)


-- | A hack to write floating point numbers in type-level like in term-level
--
-- * @i@ — integer
-- * @r@ — remainder
type i . r = i ':. r

-- | Term-level-like alias for @Rational@ constructor
--
-- * @n@ — numerator
-- * @d@ — denominator
type n % d = n ':% d


-- Local shorthands

type FP = FloatingPoint
type TR = TRational
type ToTR x = ToTRational x


data Signed a = Positive a | Negative a

type P x = 'Positive x
type N x = 'Negative x

type family ToSigned (x ∷ k) ∷ Signed a where
  ToSigned (x ∷ Nat) = P x
  ToSigned (i . r) = P (i . r)
  ToSigned (n % d) = P (n % d)
  ToSigned (P x) = P x
  ToSigned (N x) = N x

type family ShrinkSigned (x ∷ Signed k1) ∷ Signed k2 where
  ShrinkSigned (P (P x)) = ShrinkSigned (P x)
  ShrinkSigned (P (N x)) = ShrinkSigned (N x)
  ShrinkSigned (N (P x)) = ShrinkSigned (N x)
  ShrinkSigned (N (N x)) = ShrinkSigned (P x)
  ShrinkSigned x = x


-- | Convert some number to "TRational".
--
-- * @n@ — numerator
-- * @d@ — denominator
-- * @r@ — remainder
-- * @e@ — exponent
-- * @i@ — integer
-- * @r@ — remainder
type family ToTRational (a ∷ k) ∷ TRational where
  ToTRational (n % d) = n % d -- Identity
  ToTRational (x ∷ Nat) = x % 1

  ToTRational (i . r) = ToTRational (V "Transform" '(i, 1, r))

  ToTRational (V "Transform" '(n, d, 0)) = n % d
  ToTRational (V "Transform" '(n ∷ Nat, d ∷ Nat, r ∷ Nat)) =
    ToTRational (V "WithExponent" '(GetExponentFor10 0 r, n, d, r))

  ToTRational (V "WithExponent" '(e ∷ Nat, n ∷ Nat, d ∷ Nat, r ∷ Nat))
    = ((n × (10 ^ e ∷ Nat) ∷ Nat) + r ∷ Nat)
    % (d × (10 ^ e ∷ Nat) ∷ Nat)


type family GetExponentFor10 (e ∷ Nat) (r ∷ Nat) ∷ Nat where
  GetExponentFor10 e 0 = e
  GetExponentFor10 e r = GetExponentFor10 (e + 1) (r `Div` 10)


type family ToSignedTRational (a ∷ k) ∷ Signed TRational where
  ToSignedTRational (P x) = P (ToTRational x)
  ToSignedTRational (N x) = N (ToTRational x)
  ToSignedTRational (n % d) = P (ToTRational (n % d))
  ToSignedTRational (i . r) = P (ToTRational (i . r))
  ToSignedTRational (x ∷ Nat) = P (ToTRational x)


-- | Find least common denominator
--
-- Takes two denominators and returns common denominator for both.
--
-- TODO Look first for the lower value
type family LCD (ad ∷ ka) (bd ∷ kb) ∷ Nat where
  LCD (a ∷ Nat) a = a -- Denominators are equal, nothing to do
  LCD (a ∷ Nat) (b ∷ Nat) = LCD (V "Recur" 1) '(a, b)

  LCD (V "Recur" (step ∷ Nat)) '(a ∷ Nat, b ∷ Nat) =
    LCD (V "Try left" step) '((a × step) `Mod` b, a, b)

  -- Found it
  LCD (V "Try left" (step ∷ Nat)) '(0, a ∷ Nat, b ∷ Nat) =
    a × step ∷ Nat

  LCD (V "Try left" (step ∷ Nat)) '(_ ∷ Nat, a ∷ Nat, b ∷ Nat) =
    LCD (V "Try right" step) '((b × step) `Mod` a, a, b)

  -- Found it
  LCD (V "Try right" (step ∷ Nat)) '(0, a ∷ Nat, b ∷ Nat) =
    b × step ∷ Nat

  LCD (V "Try right" (step ∷ Nat)) '(_ ∷ Nat, a ∷ Nat, b ∷ Nat) =
    LCD (V "Recur" (step + 1 ∷ Nat)) '(a, b)


type family Reciprocal (a ∷ k) ∷ TRational where
  Reciprocal (a ∷ Nat) = 1 % a
  Reciprocal (i . r) = Reciprocal (ToTRational (i . r))
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

-- | Generic polymorphic type-level multiplication operator
--
-- @kr@ is one of:
--
-- * "Nat" — when @ka@ and @kb@ are both "Nat"
-- * "TRational" — when @ka@ and/or @kb@ is either "TRational" or "FloatingPoint"
-- * "Signed" of one of the above
type family (a ∷ ka) × (b ∷ kb) ∷ kr

-- Mono-kinded
type instance (a ∷ Nat) × (b ∷ Nat) = a TL.* b ∷ Nat
type instance (an % ad) × (bn % bd) = (an × bn) % (ad × bd) ∷ TR

-- FP + FP = TR
type instance (a ∷ FP) × (b ∷ FP) = ToTR a × ToTR b

-- TR + Nat → TR
type instance (a ∷ TR) × (b ∷ Nat) = a × ToTR b
type instance (a ∷ Nat) × (b ∷ TR) = b × a -- Reuse previous pattern

-- TR + FP → TR
type instance (a ∷ TR) × (b ∷ FP) = a × ToTR b
type instance (a ∷ FP) × (b ∷ TR) = b × a -- Reuse previous pattern

-- FP + Nat → TR
type instance (a ∷ FP) × (b ∷ Nat) = a × ToTR b
type instance (a ∷ Nat) × (b ∷ FP) = b × a -- Reuse previous pattern

-- Signed poly-kinded
type instance P a × P b = P (a × b)
type instance N a × N b = P (a × b)
type instance P a × N b = N (a × b)
type instance N a × P b = P b × N a

-- Singed ka + Nat → Signed kr
type instance (a ∷ Signed k) × (b ∷ Nat) = a × P b
type instance (a ∷ Nat) × (b ∷ Signed k) = b × a -- Reuse previous pattern

-- Singed ka + TR → Signed kr
type instance (a ∷ Signed k) × (b ∷ TR) = a × P b
type instance (a ∷ TR) × (b ∷ Signed k) = b × a -- Reuse previous pattern

-- Singed ka + FP → Signed kr
type instance (a ∷ Signed k) × (b ∷ FP) = a × P b
type instance (a ∷ FP) × (b ∷ Signed k) = b × a -- Reuse previous pattern


-- | Generic polymorphic type-level exponent operator
--
-- @kb@ (@exp@) is either:
--
-- * "Nat"
-- * "Signed Nat"
--
-- @kr@ is one of:
--
-- * "Nat" — when @ka@ and @kb@ are both "Nat"
-- * "TRational" — when @ka@ is "TRational" or "FloatingPoint" or ("Signed" of these)
-- * "Signed" of one of the above
type family (a ∷ ka) ^ (exp ∷ kb) ∷ kr

-- Nat + Nat = Nat
type instance (a ∷ Nat) ^ (exp ∷ Nat) = a TL.^ exp ∷ Nat

-- TR + Nat = TR
type instance (an % ad) ^ (exp ∷ Nat) = (an ^ exp) % (ad ^ exp) ∷ TR

-- FP + Nat = TR
type instance (a ∷ FP) ^ (exp ∷ Nat) = ToTR a ^ exp ∷ TR

-- Fractional exponent is not supported since it’s not supported by
-- term-level (^) operator.
-- type instance a ^ (en % ed) = …
-- type instance a ^ (ei . er) = a ^ ToTR (ei . er)

-- Mind that term-level "Rational" throws an exception for negative exponent.
type instance P a ^ P exp = P (a ^ exp)
type instance N a ^ N exp =
  If (Even exp) (P (Reciprocal a ^ exp)) (N (Reciprocal a ^ exp))
type instance P a ^ N exp = P (Reciprocal a ^ exp)
type instance N a ^ P exp = If (Even exp) (P (a ^ exp)) (N (a ^ exp))

type instance (a ∷ Signed ka) ^ (exp ∷ Nat) = a ^ P exp
type instance (a ∷ Nat) ^ (exp ∷ Signed kb) = P a ^ exp


-- | Generic polymorphic type-level division operator
--
-- @kr@ is one of:
--
-- * "TRational"
-- * "Signed TRational"
type family (a ∷ ka) ÷ (b ∷ kb) ∷ kr

-- Mono-kinded (ka ≡ kb)
type instance (an % ad) ÷ (bn % bd) = (an × bd) % (ad × bn) ∷ TR
type instance (a ∷ FP) ÷ (b ∷ FP) = ToTR a ÷ ToTR b ∷ TR
type instance (a ∷ Nat) ÷ (b ∷ Nat) = ToTR a ÷ ToTR b ∷ TR

type instance (a ∷ TR) ÷ (b ∷ FP) = a ÷ ToTR b
type instance (a ∷ FP) ÷ (b ∷ TR) = ToTR a ÷ b

type instance (a ∷ TR) ÷ (b ∷ Nat) = a ÷ ToTR b
type instance (a ∷ Nat) ÷ (b ∷ TR) = ToTR a ÷ b

type instance (a ∷ FP) ÷ (b ∷ Nat) = ToTR a ÷ ToTR b
type instance (a ∷ Nat) ÷ (b ∷ FP) = ToTR a ÷ ToTR b

type instance P a ÷ P b = P (a ÷ b)
type instance P a ÷ N b = N (a ÷ b)
type instance N a ÷ N b = P (a ÷ b)
type instance N a ÷ P b = N (a ÷ b)

type instance (a ∷ Signed ka) ÷ (b ∷ Nat) = a ÷ P b
type instance (a ∷ Nat) ÷ (b ∷ Signed kr) = P a ÷ b

type instance (a ∷ Signed ka) ÷ (b ∷ TR) = a ÷ P b
type instance (a ∷ TR) ÷ (b ∷ Signed kr) = P a ÷ b

type instance (a ∷ Signed ka) ÷ (b ∷ FP) = a ÷ P b
type instance (a ∷ FP) ÷ (b ∷ Signed kr) = P a ÷ b


-- | Generic polymorphic type-level addition operator
--
-- @kr@ is one of:
--
-- * "Nat" — when @ka@ and @kb@ are both "Nat"
-- * "TRational" — when @ka@ and/or @kb@ is either "TRational" or "FloatingPoint"
-- * "Signed" of one of the above
type family (a ∷ ka) + (b ∷ kb) ∷ kr

-- Mono-kinded (ka ≡ kb)
type instance (a ∷ Nat) + (b ∷ Nat) = a TL.+ b ∷ Nat
type instance (a ∷ FP) + (b ∷ FP) = ToTR a + ToTR b ∷ TR

type instance (an % ad) + (bn % bd) =
  V "lcd" (LCD ad bd) + '(an % ad, bn % bd) ∷ TR

type instance (V "lcd" (lcd ∷ Nat)) + '(an % ad, bn % bd) =
  ((an × (lcd `Div` ad) ∷ Nat) + (bn × (lcd `Div` bd) ∷ Nat)) % lcd ∷ TR

type instance (a ∷ FP) + (b ∷ TR) = ToTR a + b
type instance (a ∷ TR) + (b ∷ FP) = b + a -- Reuse previous instance

type instance (a ∷ Nat) + (b ∷ TR) = ToTR a + b
type instance (a ∷ TR) + (b ∷ Nat) = b + a -- Reuse previous instance

type instance (a ∷ Nat) + (b ∷ FP) = ToTR a + b -- Reuse TR + FP instance
type instance (a ∷ FP) + (b ∷ Nat) = b + a -- Reuse previous instance

type instance P a + P b = P (a + b)
type instance P a + N b = a - b ∷ Signed kr
type instance N a + N b = N (a + b)
type instance N a + P b = b - a ∷ Signed kr

type instance (a ∷ Signed ka) + (b ∷ Nat) = a + P b
type instance (a ∷ Nat) + (b ∷ Signed kb) = b + a -- Reuse previous instance

type instance (a ∷ Signed ka) + (b ∷ FP) = a + P b
type instance (a ∷ FP) + (b ∷ Signed kb) = b + a -- Reuse previous instance

type instance (a ∷ Signed ka) + (b ∷ TR) = a + P b
type instance (a ∷ TR) + (b ∷ Signed kb) = b + a -- Reuse previous instance


-- | Generic polymorphic type-level subtraction operator
--
-- @kr@ is one of:
--
-- * "Nat" — when @ka@ and @kb@ are both "Nat"
-- * "TRational" — when @ka@ and/or @kb@ is either "TRational" or "FloatingPoint"
type family (a ∷ ka) - (b ∷ kb) ∷ Signed kr

type instance (a ∷ Nat) - (b ∷ Nat) = V "≥" (b ≤ a) - '(a, b) ∷ Signed Nat
type instance V "≥" 'True - '(a, b) = P (a TL.- b) ∷ Signed Nat
type instance V "≥" 'False - '(a, b) = N (b TL.- a) ∷ Signed Nat

type instance (an % ad) - (bn % bd) =
  If (ad ≡ bd)
     (V "≥" (bn ≤ an) - '(an, bn, ad))
     (V "lcd" (LCD ad bd) - '(an % ad, bn % bd) ∷ Signed TR)
     ∷ Signed TR

-- When denominator is the same for both arguments
type instance V "≥" 'True - '(an, bn, d) = P ((an TL.- bn) % d) ∷ Signed TR
type instance V "≥" 'False - '(an, bn, d) = N ((bn TL.- an) % d) ∷ Signed TR

-- When denominator is different
type instance V "lcd" (lcd ∷ Nat) - '(an % ad, bn % bd) =
  V "lcd" lcd - '( an × (lcd `Div` ad) ∷ Nat
                 , bn × (lcd `Div` bd) ∷ Nat
                 , lcd
                 ) ∷ Signed TR
type instance V "lcd" (lcd ∷ Nat) - '(an, bn, d) =
  V "≥" (bn ≤ an) - '(an, bn, d) ∷ Signed TR

type instance (a ∷ FP) - (b ∷ FP) = ToTR a - b ∷ Signed TR
type instance (a ∷ FP) - (b ∷ TR) = ToTR a - b ∷ Signed TR
type instance (a ∷ TR) - (b ∷ FP) = a - ToTR b ∷ Signed TR

type instance (a ∷ Nat) - (b ∷ TR) = ToTR a - b ∷ Signed TR
type instance (a ∷ TR) - (b ∷ Nat) = a - ToTR b ∷ Signed TR

type instance (a ∷ Nat) - (b ∷ FP) = ToTR a - b ∷ Signed TR
type instance (a ∷ FP) - (b ∷ Nat) = ToTR a - b ∷ Signed TR

type instance P a - P b = a - b
type instance P a - N b = P (a + b)
type instance N a - N b = b - a
type instance N a - P b = N (a + b)

type instance (a ∷ Signed ka) - (b ∷ Nat) = a - P b
type instance (a ∷ Nat) - (b ∷ Signed kb) = P a - b -- Reuse previous instance

type instance (a ∷ Signed ka) - (b ∷ FP) = a - P b
type instance (a ∷ FP) - (b ∷ Signed kb) = P a - b -- Reuse previous instance

type instance (a ∷ Signed ka) - (b ∷ TR) = a - P b
type instance (a ∷ TR) - (b ∷ Signed kb) = P a - b -- Reuse previous instance


-- | Generic polymorphic type-level less-or-equal comparison operator
type family (a ∷ k1) ≤ (b ∷ k2) ∷ Bool

type instance (a ∷ Nat) ≤ (b ∷ Nat) = a TL.<=? b

type instance (an % ad) ≤ (bn % bd) = V "lcd" (LCD ad bd) ≤ '(an % ad, bn % bd)

type instance V "lcd" (lcd ∷ Nat) ≤ '(an % ad, bn % bd) =
  (an × (lcd `Div` ad) ∷ Nat) ≤ (bn × (lcd `Div` bd) ∷ Nat)

type instance (a ∷ FP) ≤ (b ∷ FP) = ToTR a ≤ b -- Reuse other instances
type instance (a ∷ FP) ≤ (b ∷ TR) = ToTR a ≤ b
type instance (a ∷ TR) ≤ (b ∷ FP) = a ≤ ToTR b

type instance (a ∷ Nat) ≤ (b ∷ TR) = ToTR a ≤ b
type instance (a ∷ TR) ≤ (b ∷ Nat) = a ≤ ToTR b

type instance (a ∷ Nat) ≤ (b ∷ FP) = ToTR a ≤ b -- Reuse other instances
type instance (a ∷ FP) ≤ (b ∷ Nat) = ToTR a ≤ b -- Reuse other instances

type instance P a ≤ P b = a ≤ b
type instance P _ ≤ N _ = 'False
type instance N _ ≤ P _ = 'True
type instance N a ≤ N b = b ≤ a

type instance (a ∷ Signed ka) ≤ (b ∷ Nat) = a ≤ P b
type instance (a ∷ Nat) ≤ (b ∷ Signed kb) = P a ≤ b -- Reuse previous instance

type instance (a ∷ Signed ka) ≤ (b ∷ FP) = a ≤ P b
type instance (a ∷ FP) ≤ (b ∷ Signed kb) = P a ≤ b -- Reuse previous instance

type instance (a ∷ Signed ka) ≤ (b ∷ TR) = a ≤ P b
type instance (a ∷ TR) ≤ (b ∷ Signed kb) = P a ≤ b -- Reuse previous instance


-- * Descent

-- ** Type-level "FloatingPoint" instances

instance
  ( n % d ~ ToTRational (i . r)
  , DescendibleAs n Integer
  , DescendibleAs d Integer
  ) ⇒ DescendibleAs (i . r) Float
  where
  descendAs Proxy = rationalToFloat (descendAs $ Proxy @n) (descendAs $ Proxy @d)

instance
  ( n % d ~ ToTRational (i . r)
  , DescendibleAs n Integer
  , DescendibleAs d Integer
  ) ⇒ DescendibleAs (i . r) Double
  where
  descendAs Proxy = rationalToDouble (descendAs $ Proxy @n) (descendAs $ Proxy @d)

instance
  ( n % d ~ ToTRational (i . r)
  , DescendibleAs n Integer
  , DescendibleAs d Integer
  ) ⇒ DescendibleAs (i . r) Rational
  where
  descendAs Proxy = descendAs (Proxy @n) % descendAs (Proxy @d)


-- ** Signed type-level number instances

instance DescendibleAs a as ⇒ DescendibleAs (P a) as where
  descendAs Proxy = descendAs $ Proxy @a

instance (Num as, DescendibleAs a as) ⇒ DescendibleAs (N a) as where
  descendAs Proxy = negate $ descendAs $ Proxy @a


-- * Internal helpers

-- | Named type-level value wrapper
--
-- Helps to avoid creating extra type families and do everything in just one.
-- At the same allows you to encapsulate those “internal” patterns by just not
-- exposing this wrapper outside of the module.
data V (s ∷ Symbol) (a ∷ k)
