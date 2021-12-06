{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.Traversable
     ( Length
     , Elem, Elem_
     , NotElem, NotElem_
     , Lookup
     , MapLookup
     ) where

import GlPlayground.TypeLevel.Arithmetic (type (+))
import GlPlayground.TypeLevel.Basic
import GlPlayground.TypeLevel.Maybe (IsJust)


type family Length (list ∷ [a]) ∷ Natural where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs


type family Elem (a ∷ k) (list ∷ [k]) ∷ Bool where
  Elem a list = IsJust (Lookup a list)

type family Elem_ (a ∷ k) (list ∷ [k]) ∷ Constraint where
  Elem_ a list = Elem a list ~ 'True

type family NotElem (a ∷ k) (list ∷ [k]) ∷ Bool where
  NotElem a list = Not (Elem a list)

type family NotElem_ (a ∷ k) (list ∷ [k]) ∷ Constraint where
  NotElem_ a list = NotElem a list ~ 'True


type family Lookup (a ∷ k) (list ∷ [k]) ∷ Maybe k where
  Lookup _ '[] = 'Nothing
  Lookup x (x ': _) = 'Just x
  Lookup a (_ ': xs) = Lookup a xs


type family MapLookup (key ∷ k) (list ∷ [(k, v)]) ∷ Maybe v where
  MapLookup _ '[] = 'Nothing
  MapLookup k ( '(k, v) ': xs ) = 'Just v
  MapLookup k ( _ ': xs ) = MapLookup k xs
