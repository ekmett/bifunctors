{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Data.Bifunctor.Wrapped
( WrappedBifunctor(..)
) where

import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Functor
import Data.Bifunctor.Unsafe
import Data.Bitraversable
import Data.Coerce
import Data.Data
import Data.Functor.Classes
import GHC.Generics

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
newtype WrappedBifunctor p a b = WrapBifunctor { unwrapBifunctor :: p a b }
  deriving ( Eq, Ord, Show, Read, Generic, Generic1, Data)

instance (Eq2 p, Eq a) => Eq1 (WrappedBifunctor p a) where
  liftEq = liftEq2 (==)
  {-# inline liftEq #-}

instance Eq2 p => Eq2 (WrappedBifunctor p) where
  liftEq2 
    :: forall a b c d. 
       (a -> b -> Bool)
    -> (c -> d -> Bool)
    -> WrappedBifunctor p a c
    -> WrappedBifunctor p b d
    -> Bool
  liftEq2 = coerce (liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> p a c -> p b d -> Bool)
  {-# inline liftEq2 #-}

instance (Ord2 p, Ord a) => Ord1 (WrappedBifunctor p a) where
  liftCompare = liftCompare2 compare
  {-# inline liftCompare #-}

instance Ord2 p => Ord2 (WrappedBifunctor p) where
  liftCompare2 
    :: forall a b c d. 
       (a -> b -> Ordering)
    -> (c -> d -> Ordering)
    -> WrappedBifunctor p a c
    -> WrappedBifunctor p b d
    -> Ordering
  liftCompare2 = coerce (liftCompare2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) -> p a c -> p b d -> Ordering)
  {-# inline liftCompare2 #-}

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

instance BifunctorFunctor WrappedBifunctor where
  bifmap = \f -> WrapBifunctor #. f .# unwrapBifunctor
  {-# inline bifmap #-}

instance BifunctorMonad WrappedBifunctor where
  bireturn = WrapBifunctor
  {-# inline bireturn #-}
  bijoin = unwrapBifunctor
  {-# inline bijoin #-}

instance BifunctorComonad WrappedBifunctor where
  biextract = unwrapBifunctor
  {-# inline biextract #-}
  biduplicate = WrapBifunctor
  {-# inline biduplicate #-}

instance Bifunctor p => Bifunctor (WrappedBifunctor p) where
  first f = WrapBifunctor #. first f .# unwrapBifunctor
  {-# inline first #-}
  second f = WrapBifunctor . second f . unwrapBifunctor
  {-# inline second #-}
  bimap f g = WrapBifunctor . bimap f g . unwrapBifunctor
  {-# inline bimap #-}

instance Bifunctor p => Functor (WrappedBifunctor p a) where
  fmap f = WrapBifunctor . second f . unwrapBifunctor
  {-# inline fmap #-}

instance Biapplicative p => Biapplicative (WrappedBifunctor p) where
  bipure a b = WrapBifunctor (bipure a b)
  {-# inline bipure #-}
  WrapBifunctor fg <<*>> WrapBifunctor xy = WrapBifunctor (fg <<*>> xy)
  {-# inline (<<*>>) #-}

instance Bifoldable p => Foldable (WrappedBifunctor p a) where
  foldMap f = bifoldMap (const mempty) f . unwrapBifunctor
  {-# inline foldMap #-}

instance Bifoldable p => Bifoldable (WrappedBifunctor p) where
  bifoldMap f g = bifoldMap f g . unwrapBifunctor
  {-# inline bifoldMap #-}

instance Bitraversable p => Traversable (WrappedBifunctor p a) where
  traverse f = fmap WrapBifunctor . bitraverse pure f . unwrapBifunctor
  {-# inline traverse #-}

instance Bitraversable p => Bitraversable (WrappedBifunctor p) where
  bitraverse f g = fmap WrapBifunctor . bitraverse f g . unwrapBifunctor
  {-# inline bitraverse #-}
