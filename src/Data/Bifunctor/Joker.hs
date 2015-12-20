{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
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
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Joker g a b = Joker { runJoker :: g b }
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 708
           , Typeable
#endif
           )

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
