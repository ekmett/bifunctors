-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Flip
-- Copyright   :  (C) 2008-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Flip
  ( Flip(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- | Make a 'Bifunctor' flipping the arguments of a 'Bifunctor'.
newtype Flip p a b = Flip { runFlip :: p b a }
  deriving (Eq,Ord,Show,Read)

instance Bifunctor p => Bifunctor (Flip p) where
  first f = Flip . second f . runFlip
  {-# INLINE first #-}
  second f = Flip . first f . runFlip
  {-# INLINE second #-}
  bimap f g = Flip . bimap g f . runFlip
  {-# INLINE bimap #-}

instance Bifunctor p => Functor (Flip p a) where
  fmap f = Flip . first f . runFlip
  {-# INLINE fmap #-}

instance Biapplicative p => Biapplicative (Flip p) where
  bipure a b = Flip (bipure b a)
  {-# INLINE bipure #-}

  Flip fg <<*>> Flip xy = Flip (fg <<*>> xy)
  {-# INLINE (<<*>>) #-}

instance Bifoldable p => Bifoldable (Flip p) where
  bifoldMap f g = bifoldMap g f . runFlip
  {-# INLINE bifoldMap #-}

instance Bifoldable p => Foldable (Flip p a) where
  foldMap f = bifoldMap f (const mempty) . runFlip
  {-# INLINE foldMap #-}

instance Bitraversable p => Bitraversable (Flip p) where
  bitraverse f g = fmap Flip . bitraverse g f . runFlip
  {-# INLINE bitraverse #-}

instance Bitraversable p => Traversable (Flip p a) where
  traverse f = fmap Flip . bitraverse f pure . runFlip
  {-# INLINE traverse #-}
