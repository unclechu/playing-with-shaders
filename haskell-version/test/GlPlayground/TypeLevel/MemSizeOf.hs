{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.MemSizeOf
     ( test
     ) where

import Data.Typeable (Typeable, typeRep)

import UnliftIO.Foreign (Storable (sizeOf))

import Test.Tasty
import Test.Tasty.HUnit

import "gl-playground" GlPlayground.TypeLevel.MemSizeOf
import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Descendible
import GlPlayground.Types (Octets (..))
import GlPlayground.Utils


test ∷ TestTree
test = testGroup "MemSizeOf"
  [ testStorableSizeOfMatch
  ]


testStorableSizeOfMatch ∷ TestTree
testStorableSizeOfMatch = testCase "Every entry matches Storable sizeOf" $
  storableSizeOfMatchTestCase $ Proxy @MemSizeMap


class StorableSizeOfMatch (list ∷ [(Type, TOctets)]) where
  storableSizeOfMatchTestCase ∷ Proxy list → Assertion

instance StorableSizeOfMatch '[] where
  storableSizeOfMatchTestCase = mempty

instance
  ( StorableSizeOfMatch tail
  , Storable t
  , Typeable t
  , DescendibleAs size Octets
  ) ⇒ StorableSizeOfMatch ( '(t, size) ': tail )
  where
  storableSizeOfMatchTestCase Proxy = do
    assertEqual ("Mismatch of size for " ⋄ show (typeRep $ Proxy @t) ⋄ " type")
      (Octets $ sizeOf (undefined ∷ t))
      (descendAs $ Proxy @size)

    storableSizeOfMatchTestCase $ Proxy @tail
