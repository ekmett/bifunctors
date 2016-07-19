{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

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

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

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

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
newtype WrappedBifunctor p a b = WrapBifunctor { unwrapBifunctor :: p a b }
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Generic1
           , Typeable
#endif
           )

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 708
data WrappedBifunctorMetaData
data WrappedBifunctorMetaCons
data WrappedBifunctorMetaSel

instance Datatype WrappedBifunctorMetaData where
    datatypeName = const "WrappedBifunctor"
    moduleName = const "Data.Bifunctor.Wrapped"

instance Constructor WrappedBifunctorMetaCons where
    conName = const "WrapBifunctor"
    conIsRecord = const True

instance Selector WrappedBifunctorMetaSel where
    selName = const "unwrapBifunctor"

instance Generic1 (WrappedBifunctor p a) where
    type Rep1 (WrappedBifunctor p a) = D1 WrappedBifunctorMetaData
        (C1 WrappedBifunctorMetaCons
            (S1 WrappedBifunctorMetaSel (Rec1 (p a))))
    from1 = M1 . M1 . M1 . Rec1 . unwrapBifunctor
    to1 = WrapBifunctor . unRec1 . unM1 . unM1 . unM1
#endif

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

instance Bitraversable p => Traversable (WrappedBifunctor p a) where
  traverse f = fmap WrapBifunctor . bitraverse pure f . unwrapBifunctor
  {-# INLINE traverse #-}

instance Bitraversable p => Bitraversable (WrappedBifunctor p) where
  bitraverse f g = fmap WrapBifunctor . bitraverse f g . unwrapBifunctor
  {-# INLINE bitraverse #-}
