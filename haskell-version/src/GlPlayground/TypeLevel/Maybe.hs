{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module GlPlayground.TypeLevel.Maybe
     ( IsJust, IsJust_
     , IsNothing, IsNothing_
     ) where

import GlPlayground.TypeLevel.Basic


type family IsJust (a ∷ Maybe k) ∷ Bool where
  IsJust ('Just _) = 'True
  IsJust 'Nothing = 'False

type family IsJust_ (a ∷ Maybe k) ∷ Constraint where
  IsJust_ x = IsJust x ~ 'True

type family IsNothing (a ∷ Maybe k) ∷ Bool where
  IsNothing a = Not (IsJust a)

type family IsNothing_ (a ∷ Maybe k) ∷ Constraint where
  IsNothing_ x = IsNothing x ~ 'True
