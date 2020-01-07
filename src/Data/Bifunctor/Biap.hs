{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

#include "bifunctors-common.h"
#ifndef MIN_VERSION_semigroups
#define MIN_VERSION_semigroups(x,y,z) 0
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
module Data.Bifunctor.Biap
 ( Biap(..)
 ) where

import GHC.Generics
import qualified Control.Monad as M
import Control.Applicative
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Monoid
import Data.Traversable
#endif
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
#if LIFTED_FUNCTOR_CLASSES
import Data.Functor.Classes
#endif
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_semigroups(0,16,2)
import qualified Data.Semigroup as S
#endif
#if __GLASGOW_HASKELL__ >= 800
import qualified Control.Monad.Fail as Fail (MonadFail)
#endif

-- | Pointwise lifting of a class over two arguments, using
-- 'Biapplicative'.
--
-- Classes that can be lifted include 'Monoid', 'Num' and
-- 'Bounded'. Each method of those classes can be defined as lifting
-- themselves over each argument of 'Biapplicative'.
--
-- @
-- mempty        = bipure mempty          mempty
-- minBound      = bipure minBound        minBound
-- maxBound      = bipure maxBound        maxBound
-- fromInteger n = bipure (fromInteger n) (fromInteger n)
--
-- negate = bimap negate negate
--
-- (+)  = biliftA2 (+)  (+)
-- (<>) = biliftA2 (<>) (<>)
-- @
--
-- 'Biap' is to 'Biapplicative' as 'Data.Monoid.Ap' is to
-- 'Applicative'.
--
-- 'Biap' can be used with @DerivingVia@ to derive a numeric instance
-- for pairs:
--
-- @
-- newtype Numpair a = Np (a, a)
--  deriving (S.Semigroup, Monoid, Num, Bounded)
--  via Biap (,) a a
-- @
--
newtype Biap bi a b = Biap { getBiap :: bi a b }
 deriving ( Eq
          , Ord
          , Show
          , Read
          , Enum
          , Functor
          , Foldable
          , Traversable
          , Alternative
          , Applicative
          , Generic
#if __GLASGOW_HASKELL__ >= 706
          , Generic1
#endif
          , Monad
#if __GLASGOW_HASKELL__ >= 800
          , Fail.MonadFail
#endif
          , M.MonadPlus
#if LIFTED_FUNCTOR_CLASSES
          , Eq1
          , Ord1
#endif

          , Bifunctor
          , Biapplicative
          , Bifoldable
#if LIFTED_FUNCTOR_CLASSES
          , Eq2
          , Ord2
#endif
          )

instance Bitraversable bi => Bitraversable (Biap bi) where
 bitraverse f g (Biap as) = Biap <$> bitraverse f g as

instance (Biapplicative bi, S.Semigroup a, S.Semigroup b) => S.Semigroup (Biap bi a b) where
  (<>) = biliftA2 (S.<>) (S.<>)

instance (Biapplicative bi, Monoid a, Monoid b) => Monoid (Biap bi a b) where
  mempty = bipure mempty mempty

instance (Biapplicative bi, Bounded a, Bounded b) => Bounded (Biap bi a b) where
  minBound = bipure minBound minBound
  maxBound = bipure maxBound maxBound

instance (Biapplicative bi, Num a, Num b) => Num (Biap bi a b) where
  (+) = biliftA2 (+) (+)
  (*) = biliftA2 (*) (*)

  negate = bimap negate negate
  abs    = bimap abs    abs
  signum = bimap signum signum

  fromInteger n = bipure (fromInteger n) (fromInteger n)
