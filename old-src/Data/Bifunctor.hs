{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_semigroups
#define MIN_VERSION_semigroups(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor
  ( Bifunctor(..)
  ) where

import Control.Applicative

#if MIN_VERSION_semigroups(0,16,2)
import Data.Semigroup
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

-- | Minimal definition either 'bimap' or 'first' and 'second'

-- | Formally, the class 'Bifunctor' represents a bifunctor
-- from @Hask@ -> @Hask@.
--
-- Intuitively it is a bifunctor where both the first and second arguments are covariant.
--
-- You can define a 'Bifunctor' by either defining 'bimap' or by defining both
-- 'first' and 'second'.
--
-- If you supply 'bimap', you should ensure that:
--
-- @'bimap' 'id' 'id' ≡ 'id'@
--
-- If you supply 'first' and 'second', ensure:
--
-- @
-- 'first' 'id' ≡ 'id'
-- 'second' 'id' ≡ 'id'
-- @
--
-- If you supply both, you should also ensure:
--
-- @'bimap' f g ≡ 'first' f '.' 'second' g@
--
-- These ensure by parametricity:
--
-- @
-- 'bimap'  (f '.' g) (h '.' i) ≡ 'bimap' f h '.' 'bimap' g i
-- 'first'  (f '.' g) ≡ 'first'  f '.' 'first'  g
-- 'second' (f '.' g) ≡ 'second' f '.' 'second' g
-- @
class Bifunctor p where
  -- | Map over both arguments at the same time.
  --
  -- @'bimap' f g ≡ 'first' f '.' 'second' g@
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  {-# INLINE bimap #-}

  -- | Map covariantly over the first argument.
  --
  -- @'first' f ≡ 'bimap' f 'id'@
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  {-# INLINE first #-}

  -- | Map covariantly over the second argument.
  --
  -- @'second' ≡ 'bimap' 'id'@
  second :: (b -> c) -> p a b -> p a c
  second = bimap id
  {-# INLINE second #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL bimap | first, second #-}
#endif

instance Bifunctor (,) where
  bimap f g ~(a, b) = (f a, g b)
  {-# INLINE bimap #-}

#if MIN_VERSION_semigroups(0,16,2)
instance Bifunctor Arg where
  bimap f g (Arg a b) = Arg (f a) (g b)
#endif

instance Bifunctor ((,,) x) where
  bimap f g ~(x, a, b) = (x, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor ((,,,) x y) where
  bimap f g ~(x, y, a, b) = (x, y, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor ((,,,,) x y z) where
  bimap f g ~(x, y, z, a, b) = (x, y, z, f a, g b)
  {-# INLINE bimap #-}

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)
  {-# INLINE bimap #-}

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)
  {-# INLINE bimap #-}

#ifdef MIN_VERSION_tagged
instance Bifunctor Tagged where
  bimap _ g (Tagged b) = Tagged (g b)
  {-# INLINE bimap #-}
#endif
