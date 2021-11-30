{-# LANGUAGE DataKinds #-}
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

module GlPlayground.TypeLevel.MemSizeOf
     ( MemSizeMap
     , TOctets (..)
     ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.TypeLevel.Arithmetic
import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Descendible
import GlPlayground.Types


type MemSizeMap =
  [ '(GL.GLfloat, 'TOctets 4)
  , '(GL.GLdouble, 'TOctets 8)
  ]
  ∷ [(Type, TOctets)]


-- | Type-level equivalent of @Octets@
newtype TOctets = TOctets Nat

instance DescendibleAs (n ∷ Nat) Int ⇒ DescendibleAs ('TOctets n) Octets where
  descendAs Proxy = Octets $ descendAs $ Proxy @n


type instance 'TOctets a × 'TOctets a = 'TOctets (a × a)
