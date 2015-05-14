-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- From the Functional Pearl \"Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures\"
-- by Conor McBride.
----------------------------------------------------------------------------
module Data.Bifunctor.Clown
  ( Clown(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- | Make a 'Functor' over the first argument of a 'Bifunctor'.
newtype Clown f a b = Clown { runClown :: f a }
  deriving (Eq,Ord,Show,Read)

instance Functor f => Bifunctor (Clown f) where
  first f = Clown . fmap f . runClown
  {-# INLINE first #-}
  second _ = Clown . runClown
  {-# INLINE second #-}
  bimap f _ = Clown . fmap f . runClown
  {-# INLINE bimap #-}

instance Functor (Clown f a) where
  fmap _ = Clown . runClown
  {-# INLINE fmap #-}

instance Applicative f => Biapplicative (Clown f) where
  bipure a _ = Clown (pure a)
  {-# INLINE bipure #-}

  Clown mf <<*>> Clown mx = Clown (mf <*> mx)
  {-# INLINE (<<*>>) #-}

instance Foldable f => Bifoldable (Clown f) where
  bifoldMap f _ = foldMap f . runClown
  {-# INLINE bifoldMap #-}

instance Foldable (Clown f a) where
  foldMap _ = mempty
  {-# INLINE foldMap #-}

instance Traversable f => Bitraversable (Clown f) where
  bitraverse f _ = fmap Clown . traverse f . runClown
  {-# INLINE bitraverse #-}

instance Traversable (Clown f a) where
  traverse _ = pure . Clown . runClown
  {-# INLINE traverse #-}
