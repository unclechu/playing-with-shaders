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

module GlPlayground.TypeLevel.Maybe
     ( IsJust
     , IsNothing
     ) where

import GlPlayground.TypeLevel.Basic


type family IsJust (a ∷ Maybe k) ∷ Bool where
  IsJust ('Just _) = 'True
  IsJust 'Nothing = 'False

type family IsNothing (a ∷ Maybe k) ∷ Bool where
  IsNothing a = Not (IsJust a)
