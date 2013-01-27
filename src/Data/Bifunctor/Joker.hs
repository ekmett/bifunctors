-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Joker
-- Copyright   :  (C) 2008-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- From the Functional Pearl \"Clowns on the left of me, Jokers on the right\"
-- by Conor McBride.
----------------------------------------------------------------------------
module Data.Bifunctor.Joker
  ( Joker(..)
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

-- | Make a 'Functor' over the first argument of a 'Bifunctor'.
newtype Joker g a b = Joker { runJoker :: g b }
  deriving (Eq,Ord,Show,Read)

instance Functor g => Bifunctor (Joker g) where
  first _ = Joker . runJoker
  {-# INLINE first #-}
  second g = Joker . fmap g . runJoker
  {-# INLINE second #-}
  bimap _ g = Joker . fmap g . runJoker
  {-# INLINE bimap #-}

instance Functor g => Functor (Joker g a) where
  fmap g = Joker . fmap g . runJoker
  {-# INLINE fmap #-}

instance Applicative g => Biapplicative (Joker g) where
  bipure _ b = Joker (pure b)
  {-# INLINE bipure #-}

  Joker mf <<*>> Joker mx = Joker (mf <*> mx)
  {-# INLINE (<<*>>) #-}

instance Apply g => Biapply (Joker g) where
  Joker fg <<.>> Joker xy = Joker (fg <.> xy)
  {-# INLINE (<<.>>) #-}

instance Foldable g => Bifoldable (Joker g) where
  bifoldMap _ g = foldMap g . runJoker
  {-# INLINE bifoldMap #-}

instance Foldable g => Foldable (Joker g a) where
  foldMap g = foldMap g . runJoker
  {-# INLINE foldMap #-}

instance Traversable g => Bitraversable (Joker g) where
  bitraverse _ g = fmap Joker . traverse g . runJoker
  {-# INLINE bitraverse #-}

instance Traversable g => Traversable (Joker g a) where
  traverse g = fmap Joker . traverse g . runJoker
  {-# INLINE traverse #-}

instance Foldable1 g => Bifoldable1 (Joker g) where
  bifoldMap1 _ g = foldMap1 g . runJoker
  {-# INLINE bifoldMap1 #-}

instance Foldable1 g => Foldable1 (Joker g a) where
  foldMap1 g = foldMap1 g . runJoker
  {-# INLINE foldMap1 #-}

instance Traversable1 g => Bitraversable1 (Joker g) where
  bitraverse1 _ g = fmap Joker . traverse1 g . runJoker
  {-# INLINE bitraverse1 #-}

instance Traversable1 g => Traversable1 (Joker g a) where
  traverse1 g = fmap Joker . traverse1 g . runJoker
  {-# INLINE traverse1 #-}
