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
-- From the Functional Pearl \"Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures\"
-- by Conor McBride.
----------------------------------------------------------------------------
module Data.Bifunctor.Clown
  ( Clown(..)
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

-- | Make a 'Functor' over the first argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Clown f a b = Clown { runClown :: f a }
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
data ClownMetaData
data ClownMetaCons
data ClownMetaSel

instance Datatype ClownMetaData where
    datatypeName _ = "Clown"
    moduleName _ = "Data.Bifunctor.Clown"

instance Constructor ClownMetaCons where
    conName _ = "Clown"
    conIsRecord _ = True

instance Selector ClownMetaSel where
    selName _ = "runClown"

instance Generic1 (Clown f a) where
    type Rep1 (Clown f a) = D1 ClownMetaData (C1 ClownMetaCons
        (S1 ClownMetaSel (Rec0 (f a))))
    from1 = M1 . M1 . M1 . K1 . runClown
    to1 = Clown . unK1 . unM1 . unM1 . unM1
#endif

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
