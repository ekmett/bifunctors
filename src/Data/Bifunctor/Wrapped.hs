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
module Data.Bifunctor.Wrapped
  ( WrappedBifunctor(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
newtype WrappedBifunctor p a b = WrapBifunctor { unwrapBifunctor :: p a b }
  deriving (Eq,Ord,Show,Read)

instance Bifunctor p => Bifunctor (WrappedBifunctor p) where
  first f = WrapBifunctor . first f . unwrapBifunctor
  {-# INLINE first #-}
  second f = WrapBifunctor . second f . unwrapBifunctor
  {-# INLINE second #-}
  bimap f g = WrapBifunctor . bimap f g . unwrapBifunctor
  {-# INLINE bimap #-}

instance Bifunctor p => Functor (WrappedBifunctor p a) where
  fmap f = WrapBifunctor . second f . unwrapBifunctor
  {-# INLINE fmap #-}

instance Biapplicative p => Biapplicative (WrappedBifunctor p) where
  bipure a b = WrapBifunctor (bipure a b)
  {-# INLINE bipure #-}
  WrapBifunctor fg <<*>> WrapBifunctor xy = WrapBifunctor (fg <<*>> xy)
  {-# INLINE (<<*>>) #-}

instance Bifoldable p => Foldable (WrappedBifunctor p a) where
  foldMap f = bifoldMap (const mempty) f . unwrapBifunctor
  {-# INLINE foldMap #-}

instance Bifoldable p => Bifoldable (WrappedBifunctor p) where
  bifoldMap f g = bifoldMap f g . unwrapBifunctor
  {-# INLINE bifoldMap #-}

instance Bitraversable p => Traversable (WrappedBifunctor p a) where
  traverse f = fmap WrapBifunctor . bitraverse pure f . unwrapBifunctor
  {-# INLINE traverse #-}

instance Bitraversable p => Bitraversable (WrappedBifunctor p) where
  bitraverse f g = fmap WrapBifunctor . bitraverse f g . unwrapBifunctor
  {-# INLINE bitraverse #-}
