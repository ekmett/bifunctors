{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
module Data.Bifunctor.Monoid
( BifunctorSemigroup
, BifunctorMonoid
) where

import Data.Bifunctor.Classes

class (forall a b. Semigroup (p a b), Bifunctor' p) => BifunctorSemigroup p
instance (forall a b. Semigroup (p a b), Bifunctor' p) => BifunctorSemigroup p

class (forall a b. Monoid (p a b), Bifunctor' p) => BifunctorMonoid p
instance (forall a b. Monoid (p a b), Bifunctor' p) => BifunctorMonoid p
