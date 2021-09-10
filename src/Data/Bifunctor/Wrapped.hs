{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Unsafe
import Data.Bitraversable
import Data.Coerce
import Data.Data
import Data.Functor.Classes
import GHC.Generics
import Text.Read (Read (..))

-- | Make a 'Functor' over the second argument of a 'Bifunctor'. This also
-- makes a 'Foldable', 'Traversable', 'Eq1', 'Ord1', 'Show1', and 'Read1'
-- from a 'Bifoldable', 'Bitraversable', 'Eq2', 'Ord2', 'Show2', and 'Read2',
-- respectively.
newtype WrappedBifunctor p a b = WrapBifunctor { unwrapBifunctor :: p a b }
  deriving (Eq, Ord, Generic, Generic1, Data)
  deriving (Show, Read) via ShowRead (WrappedBifunctor p a b)

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

instance (Show2 p, Show a) => Show1 (WrappedBifunctor p a) where
  liftShowsPrec sp sl = liftShowsPrecWhatever $ liftShowsPrec2 showsPrec showList sp sl

deriving via ShowRead2 (WrappedBifunctor p) instance Show2 p => Show2 (WrappedBifunctor p)

instance (Read2 p, Read a) => Read1 (WrappedBifunctor p a) where
  liftReadPrec rp rl = liftReadPrecWhatever $ liftReadPrec2 readPrec readListPrec rp rl

deriving via ShowRead2 (WrappedBifunctor p) instance Read2 p => Read2 (WrappedBifunctor p)

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
