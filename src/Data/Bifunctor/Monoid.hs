{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright   :  (C) 2008-2023 Edward Kmett
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
module Data.Bifunctor.Monoid
( BifunctorSemigroup
, BifunctorMonoid
) where

import Data.Bifunctor.Classes

class (forall a b. Semigroup (p a b), Bifunctor' p) => BifunctorSemigroup p
instance (forall a b. Semigroup (p a b), Bifunctor' p) => BifunctorSemigroup p

class (forall a b. Monoid (p a b), Bifunctor' p) => BifunctorMonoid p
instance (forall a b. Monoid (p a b), Bifunctor' p) => BifunctorMonoid p
