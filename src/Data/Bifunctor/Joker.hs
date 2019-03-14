{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Data.Bifunctor.Joker
  ( Joker(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Biapplicative
import Data.Bifoldable
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

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Joker g a b = Joker { runJoker :: g b }
  deriving ( Eq, Ord, Show, Read, Bounded, Enum, Ix, Semigroup, Monoid
           , Storable
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Generic1
           , Typeable
#endif
           )

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 708
data JokerMetaData
data JokerMetaCons
data JokerMetaSel

instance Datatype JokerMetaData where
    datatypeName _ = "Joker"
    moduleName _ = "Data.Bifunctor.Joker"

instance Constructor JokerMetaCons where
    conName _ = "Joker"
    conIsRecord _ = True

instance Selector JokerMetaSel where
    selName _ = "runJoker"

instance Generic1 (Joker g a) where
    type Rep1 (Joker g a) = D1 JokerMetaData (C1 JokerMetaCons
        (S1 JokerMetaSel (Rec1 g)))
    from1 = M1 . M1 . M1 . Rec1 . runJoker
    to1 = Joker . unRec1 . unM1 . unM1 . unM1
#endif

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
