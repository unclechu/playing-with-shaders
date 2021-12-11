{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.Semigroup
     ( type (⋄)
     ) where

import GHC.TypeLits (AppendSymbol)

import GlPlayground.TypeLevel.Basic


type family (a ∷ κ) ⋄ (b ∷ κ) ∷ κ

type instance (a ∷ Symbol) ⋄ (b ∷ Symbol) = a `AppendSymbol` b

type instance 'Nothing ⋄ (b ∷ Maybe κ) = b
type instance (a ∷ Maybe κ) ⋄ 'Nothing = a
type instance 'Just a ⋄ 'Just b = 'Just (a ⋄ b)

type instance '[] ⋄ (b ∷ [κ]) = b
type instance (x ': xs) ⋄ ys = x ': xs ⋄ ys
