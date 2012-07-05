-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifoldable
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifoldable 
  ( Bifoldable(..)
  , bifoldr'
  , bifoldrM
  , bifoldl'
  , bifoldlM
  , bitraverse_
  , bifor_
  , bimapM_
  , biforM_
  , bisequenceA_
  , bisequence_
  , biList
  , biconcat
  , biconcatMap
  , biany
  , biall
  ) where

import Control.Applicative
import Data.Monoid

class Bifoldable p where
  bifold :: Monoid m => p m m -> m
  bifold = bifoldMap id id

  bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
  bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty

  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldr f g z t = appEndo (bifoldMap (Endo . f) (Endo . g) t) z

  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldl f g z t = appEndo (getDual (bifoldMap (Dual . Endo . flip f) (Dual . Endo . flip g) t)) z

instance Bifoldable (,) where
  bifoldMap f g (a, b) = f a `mappend` g b

instance Bifoldable Either where
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b

bifoldr' :: Bifoldable t => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g z0 xs = bifoldl f' g' id xs z0 where 
  f' k x z = k $! f x z
  g' k x z = k $! g x z

bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g z0 xs = bifoldl f' g' return xs z0 where
  f' k x z = f x z >>= k
  g' k x z = g x z >>= k

bifoldl':: Bifoldable t => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g z0 xs = bifoldr f' g' id xs z0 where
  f' x k z = k $! f z x 
  g' x k z = k $! g z x

bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a 
bifoldlM f g z0 xs = bifoldr f' g' return xs z0 where
  f' x k z = f z x >>= k
  g' x k z = g z x >>= k
  
bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr ((*>) . f) ((*>) . g) (pure ())

bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ t f g = bitraverse_ f g t

bimapM_:: (Bifoldable t, Monad m) => (a -> m c) -> (b -> m d) -> t a b -> m ()
bimapM_ f g = bifoldr ((>>) . f) ((>>) . g) (return ())

biforM_ :: (Bifoldable t, Monad m) => t a b ->  (a -> m c) -> (b -> m d) -> m ()
biforM_ t f g = bimapM_ f g t

bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bifoldr (*>) (*>) (pure ())

bisequence_ :: (Bifoldable t, Monad m) => t (m a) (m b) -> m ()
bisequence_ = bifoldr (>>) (>>) (return ())

biList :: Bifoldable t => t a a -> [a]
biList = bifoldr (:) (:) []

biconcat :: Bifoldable t => t [a] [a] -> [a]
biconcat = bifold

biconcatMap :: Bifoldable t => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap 

biany :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny . bifoldMap (Any . p) (Any . q)

biall :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll . bifoldMap (All . p) (All . q)
