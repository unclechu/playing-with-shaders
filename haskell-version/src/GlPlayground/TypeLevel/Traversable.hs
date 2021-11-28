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

module GlPlayground.TypeLevel.Traversable
     ( Length
     , Elem
     , NotElem
     , Lookup
     , MapLookup
     ) where

import GlPlayground.TypeLevel.Arithmetic (type (+))
import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Maybe (IsJust)


type family Length (list ∷ [a]) ∷ Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs


type family Elem (a ∷ k) (list ∷ [k]) ∷ Bool where
  Elem a list = IsJust (Lookup a list)

type family NotElem (a ∷ k) (list ∷ [k]) ∷ Bool where
  NotElem a list = Not (Elem a list)


type family Lookup (a ∷ k) (list ∷ [k]) ∷ Maybe k where
  Lookup _ '[] = 'Nothing
  Lookup x (x ': _) = 'Just x
  Lookup a (_ ': xs) = Lookup a xs


type family MapLookup (key ∷ k) (list ∷ [(k, v)]) ∷ Maybe v where
  MapLookup _ '[] = 'Nothing
  MapLookup k ( '(k, v) ': xs ) = 'Just v
  MapLookup k ( _ ': xs ) = MapLookup k xs
