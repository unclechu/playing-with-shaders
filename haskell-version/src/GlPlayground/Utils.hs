{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.Utils
     ( (⋄), (∘), (•), (↤), (↦), (↜), (↝), (↫), (↬), type (×)
     , module Data.Eq.Unicode
     , module Data.Bool.Unicode
     , module Prelude.Unicode
     , module Data.Function

     -- * Async stuff
     , inBackground
     , inBackgroundBound
     ) where

import GHC.TypeLits

import Data.Bool.Unicode ((∧), (∨))
import Data.Eq.Unicode ((≡), (≠))
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Prelude.Unicode ((×), (÷))

import Control.Applicative ((<**>))
import Control.Monad (void)

import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.Async (Async, async, asyncBound)
import UnliftIO.Exception (SomeException, catch)

import System.IO (hPutStrLn, stderr)


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

type a × b = a * b


-- * Async stuff

-- | Run in background thread without waiting for it
--
-- Reports to stderr if background thread fails with an exception.
inBackground ∷ MonadUnliftIO m ⇒ m () → m ()
inBackground = inBackgroundGeneric async

-- | Run in background thread without waiting for it
--   but use @forkOS@ instead of @forkIO@
--
-- Reports to stderr if background thread fails with an exception.
inBackgroundBound ∷ MonadUnliftIO m ⇒ m () → m ()
inBackgroundBound = inBackgroundGeneric asyncBound

inBackgroundGeneric ∷ MonadUnliftIO m ⇒ (m () → m (Async ())) → m () → m ()
inBackgroundGeneric asyncFn m =
  void ∘ asyncFn $ m `catch` \(e ∷ SomeException) →
    liftIO ∘ hPutStrLn stderr $
      "Background thread failed with this exception: " ⋄ show e
