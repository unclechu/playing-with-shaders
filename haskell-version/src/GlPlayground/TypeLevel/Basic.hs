{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.Basic
     ( module Data.Type.Bool
     , module Data.Type.Equality
     , module Data.Proxy
     , module Data.Kind

     , Nat, Natural, KnownNat, natVal
     , Symbol, KnownSymbol, symbolVal

     -- * Unicode type-level operators
     , type (≡), type (≠)
     ) where

import GHC.TypeLits
  ( Nat, Natural, KnownNat, natVal
  , Symbol, KnownSymbol, symbolVal
  )

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Bool
import Data.Type.Equality


-- * Unicode type-level operators

type a ≡ b = a == b
type a ≠ b = Not (a ≡ b)
