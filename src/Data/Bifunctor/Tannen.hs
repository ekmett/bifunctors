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
module Data.Bifunctor.Tannen
  ( Tannen(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Monoid
import Data.Traversable

-- | Compose a 'Functor' on the outside of a 'Bifunctor'.
newtype Tannen f p a b = Tannen { runTannen :: f (p a b) }
  deriving (Eq,Ord,Show,Read)

instance (Functor f, Bifunctor p) => Bifunctor (Tannen f p) where
  first f = Tannen . fmap (first f) . runTannen
  {-# INLINE first #-}
  second f = Tannen . fmap (second f) . runTannen
  {-# INLINE second #-}
  bimap f g = Tannen . fmap (bimap f g) . runTannen
  {-# INLINE bimap #-}

instance (Functor f, Bifunctor p) => Functor (Tannen f p a) where
  fmap f = Tannen . fmap (second f) . runTannen
  {-# INLINE fmap #-}

instance (Applicative f, Biapplicative p) => Biapplicative (Tannen f p) where
  bipure a b = Tannen (pure (bipure a b))
  {-# INLINE bipure #-}

  Tannen fg <<*>> Tannen xy = Tannen ((<<*>>) <$> fg <*> xy)
  {-# INLINE (<<*>>) #-}

instance (Foldable f, Bifoldable p) => Foldable (Tannen f p a) where
  foldMap f = foldMap (bifoldMap (const mempty) f) . runTannen
  {-# INLINE foldMap #-}

instance (Foldable f, Bifoldable p) => Bifoldable (Tannen f p) where
  bifoldMap f g = foldMap (bifoldMap f g) . runTannen
  {-# INLINE bifoldMap #-}

instance (Traversable f, Bitraversable p) => Traversable (Tannen f p a) where
  traverse f = fmap Tannen . traverse (bitraverse pure f) . runTannen
  {-# INLINE traverse #-}

instance (Traversable f, Bitraversable p) => Bitraversable (Tannen f p) where
  bitraverse f g = fmap Tannen . traverse (bitraverse f g) . runTannen
  {-# INLINE bitraverse #-}
