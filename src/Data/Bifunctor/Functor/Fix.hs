{-# Language CPP #-}
{-# Language PolyKinds #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveLift #-}
{-# Language DeriveTraversable #-}
{-# Language DerivingVia #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language InstanceSigs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language Trustworthy #-}
{-# Language QuantifiedConstraints #-}
{-# Language UndecidableInstances #-}
-- {-# Language UndecidableSuperClasses #-}

-- | Fix points of functors over profunctors

module Data.Bifunctor.Functor.Fix
( Fix(..)
) where

import Data.Coerce
import Data.Data
import Data.Bifunctor
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Functor
import Data.Functor.Classes
import GHC.Generics
import Data.Type.Equality (TestEquality)
import Data.Type.Coercion (TestCoercion)
import Language.Haskell.TH.Syntax (Lift)

-- Fix :: ((k1 -> k2 -> *) -> k1 -> k2 -> *) -> k1 -> k2 -> *
newtype Fix f a b = In
  { out :: f (Fix f) a b
  }
  deriving stock (Generic, Generic1)

deriving newtype instance Functor (f (Fix f) a) => Functor (Fix f a)
deriving newtype instance Foldable (f (Fix f) a) => Foldable (Fix f a)
deriving stock instance Traversable (f (Fix f) a) => Traversable (Fix f a)
deriving stock instance 
  ( Data (f (Fix f) a b)
  , Typeable i
  , Typeable j
  , Typeable f
  , Typeable a
  , Typeable b
  ) => Data (Fix f (a :: i) (b :: j))
deriving via ShowRead (Fix f a b) instance Show (f (Fix f) a b) => Show (Fix f a b)
deriving via ShowRead (Fix f a b) instance Read (f (Fix f) a b) => Read (Fix f a b)
deriving via ShowRead1 (Fix f a) instance Show1 (f (Fix f) a) => Show1 (Fix f a)
deriving via ShowRead1 (Fix f a) instance Read1 (f (Fix f) a) => Read1 (Fix f a)
deriving via ShowRead2 (Fix f) instance Show2 (f (Fix f)) => Show2 (Fix f)
deriving via ShowRead2 (Fix f) instance Read2 (f (Fix f)) => Read2 (Fix f)
deriving newtype instance Eq (f (Fix f) a b) => Eq (Fix f a b)
deriving newtype instance Ord (f (Fix f) a b) => Ord (Fix f a b)
deriving newtype instance Eq1 (f (Fix f) a) => Eq1 (Fix f a)
deriving newtype instance Ord1 (f (Fix f) a) => Ord1 (Fix f a)
deriving newtype instance Eq2 (f (Fix f)) => Eq2 (Fix f)
deriving newtype instance Ord2 (f (Fix f)) => Ord2 (Fix f)
deriving newtype instance TestEquality (f (Fix f) a) => TestEquality (Fix f a)
deriving newtype instance TestCoercion (f (Fix f) a) => TestCoercion (Fix f a)
deriving stock instance Lift (f (Fix f) a b) => Lift (Fix f a b)

-- #if __GLASGOW_HASKELL__ >= 900
instance BifunctorFunctor f => Bifunctor (Fix f) where
-- #else
--instance (BifunctorFunctor f, forall a. Functor (Fix f a)) => Bifunctor (Fix f) where
-- #endif
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> Fix f a c -> Fix f b d
  bimap = coerce (bimap :: (a -> b) -> (c -> d) -> f (Fix f) a c -> f (Fix f) b d)
  first :: forall a b c. (a -> b) -> Fix f a c -> Fix f b c
  first = coerce (first :: (a -> b) -> f (Fix f) a c -> f (Fix f) b c)
  second :: forall a c d. (c -> d) -> Fix f a c -> Fix f a d
  second = coerce (second :: (c -> d) -> f (Fix f) a c -> f (Fix f) a d)
