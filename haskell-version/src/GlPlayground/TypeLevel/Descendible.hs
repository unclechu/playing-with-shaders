{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
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

-- | Descendible type-classes
module GlPlayground.TypeLevel.Descendible
     ( Descendible (..)
     , DescendibleAs (..)
     ) where

import GHC.Float (rationalToFloat, rationalToDouble)
import GHC.Real (Ratio ((:%)))
import qualified GHC.TypeNats as TN

import Data.Ratio ((%))
import Numeric.Natural (Natural)

import qualified Graphics.Rendering.OpenGL.GL as GL

import GlPlayground.TypeLevel.Basic


class Descendible (a ∷ k) where
  descend ∷ Proxy a → k


class DescendibleAs (a ∷ k) (as ∷ Type) where
  descendAs ∷ Proxy a → as

  default descendAs :: (Descendible a, k ~ as) => Proxy a -> as
  descendAs = descend


-- * Instances

-- ** From "Nat" to term-level instances

instance KnownNat n ⇒ DescendibleAs n Integer where
  descendAs Proxy = natVal $ Proxy @n

instance KnownNat n ⇒ DescendibleAs n Natural where
  descendAs Proxy = TN.natVal $ Proxy @n

instance DescendibleAs n Integer ⇒ DescendibleAs (n ∷ Nat) Rational where
  descendAs Proxy = descendAs (Proxy @n) % 1


-- *** To hardware internal types instances

instance DescendibleAs n Integer ⇒ DescendibleAs n Int where
  descendAs Proxy = fromInteger $ descendAs $ Proxy @n

instance DescendibleAs n Integer ⇒ DescendibleAs n Word where
  descendAs Proxy = fromInteger $ descendAs $ Proxy @n


-- ** Type-level "TRational" instances
--
-- These are not in "GlPlayground.TypeLevel.Arithmetic" because "(:%)" is from
-- "Data.Ratio" so it would be an orphan instance.

instance
  ( DescendibleAs n Integer
  , DescendibleAs d Integer
  ) ⇒ DescendibleAs (n ':% d) Rational
  where
  descendAs Proxy = descendAs (Proxy @n) % descendAs (Proxy @d)

instance
  ( DescendibleAs n Integer
  , DescendibleAs d Integer
  ) ⇒ DescendibleAs (n ':% d) Float
  where
  descendAs Proxy = rationalToFloat (descendAs $ Proxy @n) (descendAs $ Proxy @d)

instance
  ( DescendibleAs n Integer
  , DescendibleAs d Integer
  ) ⇒ DescendibleAs (n ':% d) Double where
  descendAs Proxy = rationalToDouble (descendAs $ Proxy @n) (descendAs $ Proxy @d)


-- *** Some built-in types descent


-- **** Unit

instance DescendibleAs '() () where
  descendAs Proxy = ()


-- **** Bool

instance DescendibleAs 'True Bool where descendAs Proxy = True
instance DescendibleAs 'False Bool where descendAs Proxy = False


-- **** Ordering

instance DescendibleAs 'LT Ordering where descendAs Proxy = LT
instance DescendibleAs 'EQ Ordering where descendAs Proxy = EQ
instance DescendibleAs 'GT Ordering where descendAs Proxy = GT


-- **** Maybe

instance DescendibleAs 'Nothing (Maybe as) where
  descendAs Proxy = Nothing

instance DescendibleAs a as ⇒ DescendibleAs ('Just a) (Maybe as) where
  descendAs Proxy = Just $ descendAs $ Proxy @a


-- **** Either

instance DescendibleAs a l ⇒ DescendibleAs ('Left a) (Either l r) where
  descendAs Proxy = Left $ descendAs $ Proxy @a

instance DescendibleAs a r ⇒ DescendibleAs ('Right a) (Either l r) where
  descendAs Proxy = Right $ descendAs $ Proxy @a


-- *** Type-level string instances

instance KnownSymbol s ⇒ DescendibleAs s String where
  descendAs Proxy = symbolVal $ Proxy @s


-- ** GL instances

instance Descendible 'GL.VertexShader where descend Proxy = GL.VertexShader
instance DescendibleAs 'GL.VertexShader GL.ShaderType

instance Descendible 'GL.FragmentShader where descend Proxy = GL.FragmentShader
instance DescendibleAs 'GL.FragmentShader GL.ShaderType


-- ** List instances (serialization into lists)
--
-- Serialization type-level list into term-level

instance DescendibleAs '[] [as] where
  descendAs Proxy = []

instance
  ( DescendibleAs x as
  , DescendibleAs xs [as]
  ) ⇒ DescendibleAs (x ': xs) [as]
  where
  descendAs Proxy = descendAs (Proxy @x) : descendAs (Proxy @xs)
