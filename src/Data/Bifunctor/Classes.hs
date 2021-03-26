{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}

-- | @base@ has yet to adopt @QuantifiedConstraints@ for 'Bifunctor', 'Bifoldable' and 'Bitraversable'
--
-- This change is coming.
--
-- These definitions are portable even across versions of base that do not yet have this change applied.
--
-- When base 4.15 has faded sufficiently far into the past, these will eventually just be retired.

module Data.Bifunctor.Classes
( Bifunctor'
, Bifoldable'
, Bitraversable'
, Biapplicative'
) where

import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable

class (Bifunctor p, forall a. Functor (p a)) => Bifunctor' p
instance (Bifunctor p, forall a. Functor (p a)) => Bifunctor' p

class (Biapplicative p, forall a. Functor (p a)) => Biapplicative' p
instance (Biapplicative p, forall a. Functor (p a)) => Biapplicative' p

class (Bifoldable p, forall a. Foldable (p a)) => Bifoldable' p
instance (Bifoldable p, forall a. Foldable (p a)) => Bifoldable' p

class (Bitraversable p, forall a. Traversable (p a)) => Bitraversable' p
instance (Bitraversable p, forall a. Traversable (p a)) => Bitraversable' p
