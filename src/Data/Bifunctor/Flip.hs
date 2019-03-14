{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Flip
-- Copyright   :  (C) 2008-2016 Edward Kmett
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

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Functor
import Data.Bitraversable

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif

import Data.Ix

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Data.Semigroup

#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

import Foreign.Storable

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

-- | Make a 'Bifunctor' flipping the arguments of a 'Bifunctor'.
newtype Flip p a b = Flip { runFlip :: p b a }
  deriving ( Eq, Ord, Show, Read, Bounded, Enum, Ix, Semigroup, Monoid
           , Storable
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Typeable
#endif
           )

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

instance BifunctorFunctor Flip where
  bifmap f (Flip p) = Flip (f p)
