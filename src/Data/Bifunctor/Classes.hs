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
-- When base 4.15 has faded sufficiently far into the past, these will eventually just re-export @base@

module Data.Bifunctor.Classes
( Bifunctor', module Data.Bifunctor
, Bifoldable', module Data.Bifoldable
, Bitraversable', module Data.Bitraversable
) where

import Data.Bifunctor hiding (Bifunctor)
import qualified Data.Bifunctor as Base
import Data.Bifoldable hiding (Bifoldable)
import qualified Data.Bifoldable as Base
import Data.Bitraversable hiding (Bitraversable)
import qualified Data.Bitraversable as Base

class (Base.Bifunctor p, forall a. Functor (p a)) => Bifunctor' p
instance (Base.Bifunctor p, forall a. Functor (p a)) => Bifunctor' p

class (Base.Bifoldable p, forall a. Foldable (p a)) => Bifoldable' p
instance (Base.Bifoldable p, forall a. Foldable (p a)) => Bifoldable' p

class (Base.Bitraversable p, forall a. Traversable (p a)) => Bitraversable' p
instance (Base.Bitraversable p, forall a. Traversable (p a)) => Bitraversable' p
