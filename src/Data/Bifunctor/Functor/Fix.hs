{-# LANGUAGE PolyKinds #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveTraversable #-}
{-# Language DerivingStrategies #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language InstanceSigs #-}
{-# Language Trustworthy #-}
{-# Language TypeApplications #-}
{-# Language UndecidableInstances #-}

-- | Fix points of functors over profunctors

module Data.Bifunctor.Functor.Fix
( Fix(..)
) where

import Data.Coerce
import Data.Data
import Data.Bifunctor
import Data.Bifunctor.Functor
import GHC.Generics

-- Fix :: ((k1 -> k2 -> *) -> k1 -> k2 -> *) -> k1 -> k2 -> *
newtype Fix f a b = In
  { out :: f (Fix f) a b
  } deriving (Generic, Generic1)

deriving stock instance Functor (f (Fix f) a) => Functor (Fix f a)
deriving stock instance Foldable (f (Fix f) a) => Foldable (Fix f a)
deriving stock instance Traversable (f (Fix f) a) => Traversable (Fix f a)
deriving stock instance 
  ( Data (f (Fix f) a b)
  , Typeable i
  , Typeable j
  , Typeable f
  , Typeable a
  , Typeable b
  ) => Data (Fix f (a :: i) (b :: j))

instance BifunctorFunctor f => Bifunctor (Fix f) where
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> Fix f a c -> Fix f b d
  bimap = coerce (bimap :: (a -> b) -> (c -> d) -> f (Fix f) a c -> f (Fix f) b d)
  first :: forall a b c. (a -> b) -> Fix f a c -> Fix f b c
  first = coerce (first :: (a -> b) -> f (Fix f) a c -> f (Fix f) b c)
  second :: forall a c d. (c -> d) -> Fix f a c -> Fix f a d
  second = coerce (second :: (c -> d) -> f (Fix f) a c -> f (Fix f) a d)
