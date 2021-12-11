{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.Monoid
     ( Mempty
     , Mconcat
     ) where

import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Semigroup


type family Mempty ∷ κ

type instance Mempty = "" ∷ Symbol
type instance Mempty = '[] ∷ [κ]


type family Mconcat (list ∷ [κ]) ∷ κ

type instance Mconcat '[] = Mempty
type instance Mconcat (x ': xs) = x ⋄ Mconcat xs
