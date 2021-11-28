{-# LANGUAGE DataKinds #-}
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
     ) where

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.TypeLevel.Basic


type MemSizeMap =
  [ '(GL.GLfloat, 4)
  , '(GL.GLdouble, 8)
  ]
  ∷ [(Type, Nat)]
