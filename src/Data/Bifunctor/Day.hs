{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language DerivingStrategies #-}
{-# Language Safe #-}
{-# Language TypeOperators #-}
{-# Language RoleAnnotations #-}

-- |
-- Copyright   :  (C) 2020-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Data.Bifunctor.Day 
( Day(..)
, assoc, unassoc
, lambda, unlambda
, rho, unrho
, monday
, oneday
) where

import Data.Biapplicative
import Data.Bifunctor.Functor

-- | (,) is the unit of 'Bifunctor' Day convolution
type role Day
  representational
  representational
  representational
  representational 
data Day p q a b where
  Day 
    :: (a -> c -> x)
    -> (b -> d -> y)
    -> p a b
    -> q c d
    -> Day p q x y

instance Functor (Day p q a) where
  fmap g (Day f g' p q) = Day f (\b d -> g (g' b d)) p q
  {-# inline fmap #-}

instance Bifunctor (Day p q) where
  bimap = \f g (Day f' g' p q) -> Day 
    (\a c -> f (f' a c))
    (\b d -> g (g' b d))
    p q
  {-# inline bimap #-}
  first = \f (Day f' g p q) -> Day 
    (\a c -> f (f' a c))
    g
    p q
  {-# inline first #-}
  second = \g (Day f g' p q) -> Day 
    f
    (\b d -> g (g' b d))
    p q
  {-# inline second #-}

instance Bifunctor p => BifunctorFunctor (Day p) where
  bifmap = \h (Day f g p q) -> Day f g p (h q)
  {-# inline bifmap #-}

instance Biapplicative p => BifunctorMonad (Day p) where
  bireturn = Day (\_ x -> x) (\_ x -> x) biempty
  {-# inline bireturn #-}
  bijoin = \(Day f g p (Day h i p' q)) ->
    Day 
      (\(a1,a2) c1 -> f a1 (h a2 c1)) 
      (\(b1,b2) d1 -> g b1 (i b2 d1))
      (biappend p p') 
      q
  {-# inline bijoin #-}

assoc :: Day (Day p q) r :-> Day p (Day q r)
assoc = \(Day f g (Day h i p q) r) ->
  Day 
    (\a2 (c1,c) -> f (h a2 c1) c)
    (\a2 (c1,c) -> g (i a2 c1) c)
    p 
    (Day (,) (,) q r)
{-# inline assoc #-}

unassoc :: Day p (Day q r) :-> Day (Day p q) r
unassoc = \(Day f g p (Day h i q r)) ->
  Day 
    (\(a1,a2) c1 -> f a1 (h a2 c1))
    (\(a1,a2) c1 -> g a1 (i a2 c1))
    (Day (,) (,) p q)
    r
{-# inline unassoc #-}

unit :: ((),())
unit = ((),())
{-# noinline[1] unit #-}

lambda :: p :-> Day (,) p
lambda = Day (\_ a -> a) (\_ a -> a) unit
{-# inline lambda #-}

unlambda :: Bifunctor p => Day (,) p :-> p
unlambda (Day f g (a,b) q) = bimap (f a) (g b) q
{-# inline unlambda #-}

rho :: p :-> Day p (,)
rho p = Day const const p unit
{-# inline rho #-}

unrho :: Bifunctor p => Day p (,) :-> p
unrho (Day f g p (a,b)) = bimap (`f` a) (`g` b) p
{-# inline unrho #-}

monday :: Biapplicative p => Day p p :-> p
monday = \(Day f g p q) -> biliftA2 f g p q
{-# inline monday #-}

oneday :: Biapplicative p => (,) :-> p
oneday = uncurry bipure 
{-# inline oneday #-}
