{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Utils
     ( (⋄), (∘), (•), (↤), (↦), (↜), (↝), (↫), (↬)
     , module Data.Eq.Unicode
     , module Data.Bool.Unicode
     , module Prelude.Unicode
     ) where

import Data.Functor ((<&>), ($>))
import Control.Applicative ((<**>))
import Data.Eq.Unicode ((≡), (≠))
import Data.Bool.Unicode ((∧), (∨))
import Prelude.Unicode ((×), (÷))


{-# INLINE (⋄) #-}
(⋄) ∷ Semigroup a ⇒ a → a → a
(⋄) = (<>)
infixr 6 ⋄

{-# INLINE (∘) #-}
(∘) ∷ Functor f ⇒ (a → b) → f a → f b
(∘) = (<$>)
infixl 4 ∘
-- infixl 9 ∘ -- Precedence taken from (.) dot operator

{-# INLINE (•) #-}
(•) ∷ Functor f ⇒ f a → (a → b) → f b
(•) = (<&>)
infixl 1 •

{-# INLINE (↤) #-}
(↤) ∷ Functor f ⇒ a → f b → f a
(↤) = (<$)
infixl 4 ↤

{-# INLINE (↦) #-}
(↦) ∷ Functor f ⇒ f a → b → f b
(↦) = ($>)
infixl 4 ↦

{-# INLINE (↜) #-}
(↜) ∷ Applicative f ⇒ f (a → b) → f a → f b
(↜) = (<*>)
infixl 4 ↜

{-# INLINE (↝) #-}
(↝) ∷ Applicative f ⇒ f a → f (a → b) → f b
(↝) = (<**>)
infixl 4 ↝

{-# INLINE (↫) #-}
(↫) ∷ Applicative f ⇒ f a → f b → f a
(↫) = (<*)
infixl 4 ↫

{-# INLINE (↬) #-}
(↬) ∷ Applicative f ⇒ f a → f b → f b
(↬) = (*>)
infixl 4 ↬
