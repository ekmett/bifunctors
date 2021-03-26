{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Data.Bifunctor.Biap
( Biap(..)
) where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail (MonadFail)
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Classes
import GHC.Generics
import qualified Data.Semigroup as S
import Numeric

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
--  deriving (Semigroup, Monoid, Num, Fractional, Floating, Bounded)
--  via Biap (,) a a
-- @
--
newtype Biap bi a b = Biap { getBiap :: bi a b }
  deriving stock
  ( Eq
  , Ord
  , Show
  , Read
  , Functor
  , Foldable
  , Traversable
  , Generic
  , Generic1
  )
  deriving newtype
  ( Alternative
  , Applicative
  -- @since 6: Enum removed, it isn't compatible with Bounded
  , Monad
  , Fail.MonadFail
  , MonadPlus
  , Eq1
  , Ord1
  , Bifunctor
  , Biapplicative
  , Bifoldable
  , Eq2
  , Ord2
  )

-- TODO: Show1, Show2, Read1, Read2

instance Bitraversable bi => Bitraversable (Biap bi) where
  bitraverse f g (Biap as) = Biap <$> bitraverse f g as
  {-# inline bitraverse #-}

instance (Biapplicative bi, S.Semigroup a, S.Semigroup b) => S.Semigroup (Biap bi a b) where
  (<>) = biliftA2 (S.<>) (S.<>)
  {-# inline (<>) #-}

instance (Biapplicative bi, Monoid a, Monoid b) => Monoid (Biap bi a b) where
  mempty = bipure mempty mempty
  {-# inline mempty #-}

instance (Biapplicative bi, Bounded a, Bounded b) => Bounded (Biap bi a b) where
  minBound = bipure minBound minBound
  maxBound = bipure maxBound maxBound
  {-# inline minBound #-}
  {-# inline maxBound #-}

instance
  ( Biapplicative bi, Num a, Num b
  ) => Num (Biap bi a b) where
  (+) = biliftA2 (+) (+)
  (-) = biliftA2 (-) (-)
  (*) = biliftA2 (*) (*)
  negate = bimap negate negate
  abs    = bimap abs    abs
  signum = bimap signum signum
  fromInteger n = bipure (fromInteger n) (fromInteger n)
  {-# inline (+) #-}
  {-# inline (-) #-}
  {-# inline (*) #-}
  {-# inline negate #-}
  {-# inline abs #-}
  {-# inline signum #-}
  {-# inline fromInteger #-}

instance
  ( Biapplicative bi, Fractional a, Fractional b
  ) => Fractional (Biap bi a b) where
  (/) = biliftA2 (/) (/)
  recip = bimap recip recip
  fromRational r = bipure (fromRational r) (fromRational r)
  {-# inline (/) #-}
  {-# inline recip #-}
  {-# inline fromRational #-}


instance
  ( Biapplicative bi, Floating a, Floating b
  ) => Floating (Biap bi a b) where
  pi = bipure pi pi
  exp = bimap exp exp
  log = bimap log log
  sqrt = bimap sqrt sqrt
  (**) = biliftA2 (**) (**)
  logBase = biliftA2 logBase logBase
  sin = bimap sin sin
  cos = bimap cos cos
  tan = bimap tan tan
  asin = bimap asin asin
  acos = bimap acos acos
  atan = bimap atan atan
  sinh = bimap sinh sinh
  cosh = bimap cosh cosh
  tanh = bimap tanh tanh
  asinh = bimap asinh asinh
  acosh = bimap acosh acosh
  atanh = bimap atanh atanh
  log1p = bimap log1p log1p
  expm1 = bimap expm1 expm1
  log1pexp = bimap log1pexp log1pexp
  log1mexp = bimap log1mexp log1mexp
  {-# inline pi #-}
  {-# inline exp #-}
  {-# inline log #-}
  {-# inline sqrt #-}
  {-# inline (**) #-}
  {-# inline logBase #-}
  {-# inline sin #-}
  {-# inline cos #-}
  {-# inline tan #-}
  {-# inline asin #-}
  {-# inline acos #-}
  {-# inline atan #-}
  {-# inline sinh #-}
  {-# inline cosh #-}
  {-# inline tanh #-}
  {-# inline asinh #-}
  {-# inline acosh #-}
  {-# inline atanh #-}
  {-# inline log1p #-}
  {-# inline expm1 #-}
  {-# inline log1pexp #-}
  {-# inline log1mexp #-}

