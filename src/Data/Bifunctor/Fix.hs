{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Fix
-- Copyright   :  (C) 2008-2014 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Bifunctor.Fix
  ( Fix(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifunctor.Apply
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Apply
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable

-- | Greatest fixpoint of a 'Bifunctor' (a 'Functor' over the first argument, with zipping over the second argument).
newtype Fix p a = Fix { unFix :: p a (Fix p a) }

deriving instance Eq   (p a (Fix p a)) => Eq   (Fix p a)
deriving instance Ord  (p a (Fix p a)) => Ord  (Fix p a)
deriving instance Show (p a (Fix p a)) => Show (Fix p a)
deriving instance Read (p a (Fix p a)) => Read (Fix p a)

instance (Bifunctor p) => Functor (Fix p) where
	fmap f = hyloWith (bimap f) Fix unFix
	{-# INLINE fmap #-}

instance (Biapplicative p) => Applicative (Fix p) where
	pure = hyloWith (bipure <*>) Fix id
	{-# INLINE pure #-}
	(<*>) = hyloWith2 (biliftA2 ($)) Fix unFix unFix
	{-# INLINE (<*>) #-}

instance (Biapply p) => Apply (Fix p) where
	(<.>) = hyloWith2 (bilift2 ($)) Fix unFix unFix
	{-# INLINE (<.>) #-}

instance (Bifoldable p) => Foldable (Fix p) where
	foldMap f = hyloWith (bifoldMap f) id unFix
	{-# INLINE foldMap #-}

instance (Bifoldable1 p) => Foldable1 (Fix p) where
	foldMap1 f = hyloWith (bifoldMap1 f) id unFix
	{-# INLINE foldMap1 #-}

instance (Bitraversable p) => Traversable (Fix p) where
	traverse f = hyloWith (bitraverse f) (fmap Fix) unFix
	{-# INLINE traverse #-}

instance (Bitraversable1 p) => Traversable1 (Fix p) where
	traverse1 f = hyloWith (bitraverse1 f) (fmap Fix) unFix
	{-# INLINE traverse1 #-}

hyloWith :: ((a -> b) -> s -> t) -> (t -> b) -> (a -> s) -> a -> b
hyloWith f tb as = ab
	where
		ab a = tb (f ab (as a))
{-# INLINE hyloWith #-}

hyloWith2 :: ((a -> b -> c) -> s -> t -> u) -> (u -> c) -> (a -> s) -> (b -> t) -> a -> b -> c
hyloWith2 f uc as bt = abc
	where
		abc a b = uc (f abc (as a) (bt b))
{-# INLINE hyloWith2 #-}

-- hyloWith3 :: ((a -> b -> c -> d) -> s -> t -> u -> v) -> (v -> d) -> (a -> s) -> (b -> t) -> (c -> u) -> a -> b -> c -> d
-- hyloWith3 f vd as bt cu = abcd
-- 	where
-- 		abcd a b c = vd (f abcd (as a) (bt b) (cu c))
-- {-# INLINE hyloWith3 #-}
-- 
-- hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
-- hylo = hyloWith fmap
-- {-# INLINE hylo #-}
-- 
-- hyloF2 :: (Apply f) => (f c -> c) -> (a -> f a) -> (b -> f b) -> a -> b -> c
-- hyloF2 = hyloWith2 liftF2
-- {-# INLINE hyloF2 #-}
-- 
-- hyloF3 :: (Apply f) => (f d -> d) -> (a -> f a) -> (b -> f b) -> (c -> f c) -> a -> b -> c -> d
-- hyloF3 = hyloWith3 liftF3
-- {-# INLINE hyloF3 #-}
-- 
-- hyloA :: (Applicative f) => (f b -> b) -> (a -> f a) -> a -> b
-- hyloA = hyloWith liftA
-- {-# INLINE hyloA #-}
-- 
-- hyloA2 :: (Applicative f) => (f c -> c) -> (a -> f a) -> (b -> f b) -> a -> b -> c
-- hyloA2 = hyloWith2 liftA2
-- {-# INLINE hyloA2 #-}
-- 
-- hyloA3 :: (Applicative f) => (f d -> d) -> (a -> f a) -> (b -> f b) -> (c -> f c) -> a -> b -> c -> d
-- hyloA3 = hyloWith3 liftA3
-- {-# INLINE hyloA3 #-}
