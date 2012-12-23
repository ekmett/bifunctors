-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor
-- Copyright   :  (C) 2008-2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor (Bifunctor(..)) where

import Control.Applicative

-- | Minimal definition either 'bimap' or 'first' and 'second'
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  {-# INLINE bimap #-}

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  {-# INLINE first #-}

  second :: (b -> c) -> p a b -> p a c
  second = bimap id
  {-# INLINE second #-}

instance Bifunctor (,) where
  bimap f g ~(a, b) = (f a, g b)
  {-# INLINE bimap #-}

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
