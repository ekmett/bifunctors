-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Biff
-- Copyright   :  (C) 2008-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Biff
  ( Biff(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifunctor.Apply
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Apply
import Data.Monoid
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable

-- | Compose two 'Functor's on the inside of a 'Bifunctor'.
newtype Biff p f g a b = Biff { runBiff :: p (f a) (g b) }
  deriving (Eq,Ord,Show,Read)

instance (Bifunctor p, Functor f, Functor g) => Bifunctor (Biff p f g) where
  first f = Biff . first (fmap f) . runBiff
  {-# INLINE first #-}
  second f = Biff . second (fmap f) . runBiff
  {-# INLINE second #-}
  bimap f g = Biff . bimap (fmap f) (fmap g) . runBiff
  {-# INLINE bimap #-}

instance (Bifunctor p, Functor g) => Functor (Biff p f g a) where
  fmap f = Biff . second (fmap f) . runBiff
  {-# INLINE fmap #-}

instance (Biapply p, Apply f, Apply g) => Biapply (Biff p f g) where
  Biff fg <<.>> Biff xy = Biff (bimap (<.>) (<.>) fg <<.>> xy)
  {-# INLINE (<<.>>) #-}

instance (Biapplicative p, Applicative f, Applicative g) => Biapplicative (Biff p f g) where
  bipure a b = Biff (bipure (pure a) (pure b))
  {-# INLINE bipure #-}

  Biff fg <<*>> Biff xy = Biff (bimap (<*>) (<*>) fg <<*>> xy)
  {-# INLINE (<<*>>) #-}

instance (Bifoldable p, Foldable g) => Foldable (Biff p f g a) where
  foldMap f = bifoldMap (const mempty) (foldMap f) . runBiff
  {-# INLINE foldMap #-}

instance (Bifoldable p, Foldable f, Foldable g) => Bifoldable (Biff p f g) where
  bifoldMap f g = bifoldMap (foldMap f) (foldMap g) . runBiff
  {-# INLINE bifoldMap #-}

instance (Bitraversable p, Traversable g) => Traversable (Biff p f g a) where
  traverse f = fmap Biff . bitraverse pure (traverse f) . runBiff
  {-# INLINE traverse #-}

instance (Bitraversable p, Traversable f, Traversable g) => Bitraversable (Biff p f g) where
  bitraverse f g = fmap Biff . bitraverse (traverse f) (traverse g) . runBiff
  {-# INLINE bitraverse #-}

instance (Bifoldable1 p, Foldable1 f, Foldable1 g) => Bifoldable1 (Biff p f g) where
  bifoldMap1 f g = bifoldMap1 (foldMap1 f) (foldMap1 g) . runBiff
  {-# INLINE bifoldMap1 #-}

instance (Bitraversable1 p, Traversable1 f, Traversable1 g) => Bitraversable1 (Biff p f g) where
  bitraverse1 f g = fmap Biff . bitraverse1 (traverse1 f) (traverse1 g) . runBiff
  {-# INLINE bitraverse1 #-}
