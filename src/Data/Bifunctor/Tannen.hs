{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DeriveDataTypeable #-}
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
module Data.Bifunctor.Tannen
  ( Tannen(..)
  ) where

import Control.Applicative

import Control.Arrow as A
import Control.Category

import Data.Bifunctor as B
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Monoid
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

import Prelude hiding ((.),id)

-- | Compose a 'Functor' on the outside of a 'Bifunctor'.
newtype Tannen f p a b = Tannen { runTannen :: f (p a b) }
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 708
           , Typeable
#endif
           )

instance (Functor f, Bifunctor p) => Bifunctor (Tannen f p) where
  first f = Tannen . fmap (B.first f) . runTannen
  {-# INLINE first #-}
  second f = Tannen . fmap (B.second f) . runTannen
  {-# INLINE second #-}
  bimap f g = Tannen . fmap (bimap f g) . runTannen
  {-# INLINE bimap #-}

instance (Functor f, Bifunctor p) => Functor (Tannen f p a) where
  fmap f = Tannen . fmap (B.second f) . runTannen
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

instance (Applicative f, Category p) => Category (Tannen f p) where
  id = Tannen $ pure id
  Tannen fpbc . Tannen fpab = Tannen $ liftA2 (.) fpbc fpab

instance (Applicative f, Arrow p) => Arrow (Tannen f p) where
  arr f = Tannen $ pure $ arr f
  first = Tannen . fmap A.first . runTannen
  second = Tannen . fmap A.second . runTannen
  Tannen ab *** Tannen cd = Tannen $ liftA2 (***) ab cd
  Tannen ab &&& Tannen ac = Tannen $ liftA2 (&&&) ab ac

instance (Applicative f, ArrowChoice p) => ArrowChoice (Tannen f p) where
  left  = Tannen . fmap left . runTannen
  right = Tannen . fmap right . runTannen
  Tannen ab +++ Tannen cd = Tannen $ liftA2 (+++) ab cd
  Tannen ac ||| Tannen bc = Tannen $ liftA2 (|||) ac bc

instance (Applicative f, ArrowLoop p) => ArrowLoop (Tannen f p) where
  loop = Tannen . fmap loop . runTannen

instance (Applicative f, ArrowZero p) => ArrowZero (Tannen f p) where
  zeroArrow = Tannen $ pure zeroArrow

instance (Applicative f, ArrowPlus p) => ArrowPlus (Tannen f p) where
  Tannen f <+> Tannen g = Tannen (liftA2 (<+>) f g)

