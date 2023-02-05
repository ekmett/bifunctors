{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
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

import Data.Biapplicative
import Data.Bifoldable
import Data.Bifoldable1 (Bifoldable1(..))
import Data.Bitraversable
import Data.Functor.Classes
import GHC.Generics

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
newtype WrappedBifunctor p a b = WrapBifunctor { unwrapBifunctor :: p a b }
  deriving (Eq, Ord, Show, Read, Generic, Generic1)

instance (Eq2 p, Eq a) => Eq1 (WrappedBifunctor p a) where
  liftEq = liftEq2 (==)
instance Eq2 p => Eq2 (WrappedBifunctor p) where
  liftEq2 f g (WrapBifunctor x) (WrapBifunctor y) = liftEq2 f g x y

instance (Ord2 p, Ord a) => Ord1 (WrappedBifunctor p a) where
  liftCompare = liftCompare2 compare
instance Ord2 p => Ord2 (WrappedBifunctor p) where
  liftCompare2 f g (WrapBifunctor x) (WrapBifunctor y) = liftCompare2 f g x y

instance (Read2 p, Read a) => Read1 (WrappedBifunctor p a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
instance Read2 p => Read2 (WrappedBifunctor p) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 p = readParen (p > 10) $ \s0 -> do
    ("WrapBifunctor",   s1) <- lex s0
    ("{",               s2) <- lex s1
    ("unwrapBifunctor", s3) <- lex s2
    (x,                 s4) <- liftReadsPrec2 rp1 rl1 rp2 rl2 0 s3
    ("}",               s5) <- lex s4
    return (WrapBifunctor x, s5)

instance (Show2 p, Show a) => Show1 (WrappedBifunctor p a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Show2 p => Show2 (WrappedBifunctor p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (WrapBifunctor x) = showParen (p > 10) $
      showString "WrapBifunctor {unwrapBifunctor = "
    . liftShowsPrec2 sp1 sl1 sp2 sl2 0 x
    . showChar '}'

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

instance Bifoldable1 p => Bifoldable1 (WrappedBifunctor p) where
  bifoldMap1 f g = bifoldMap1 f g . unwrapBifunctor
  {-# INLINE bifoldMap1 #-}

instance Bitraversable p => Traversable (WrappedBifunctor p a) where
  traverse f = fmap WrapBifunctor . bitraverse pure f . unwrapBifunctor
  {-# INLINE traverse #-}

instance Bitraversable p => Bitraversable (WrappedBifunctor p) where
  bitraverse f g = fmap WrapBifunctor . bitraverse f g . unwrapBifunctor
  {-# INLINE bitraverse #-}
