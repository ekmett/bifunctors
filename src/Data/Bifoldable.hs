{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

#ifndef MIN_VERSION_semigroups
#define MIN_VERSION_semigroups(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
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
import Data.Functor.Constant

#if MIN_VERSION_semigroups(0,16,2)
import Data.Semigroup
#else
import Data.Monoid
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
import Data.Typeable
#endif

-- | Minimal definition either 'bifoldr' or 'bifoldMap'

-- | 'Bifoldable' identifies foldable structures with two different varieties of
-- elements. Common examples are 'Either' and '(,)':
--
-- > instance Bifoldable Either where
-- >   bifoldMap f _ (Left  a) = f a
-- >   bifoldMap _ g (Right b) = g b
-- >
-- > instance Bifoldable (,) where
-- >   bifoldr f g z (a, b) = f a (g b z)
--
-- When defining more than the minimal set of definitions, one should ensure
-- that the following identities hold:
--
-- @
-- 'bifold' ≡ 'bifoldMap' 'id' 'id'
-- 'bifoldMap' f g ≡ 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'
-- 'bifoldr' f g z t ≡ 'appEndo' ('bifoldMap' (Endo . f) (Endo . g) t) z
-- @
class Bifoldable p where
  -- | Combines the elements of a structure using a monoid.
  --
  -- @'bifold' ≡ 'bifoldMap' 'id' 'id'@
  bifold :: Monoid m => p m m -> m
  bifold = bifoldMap id id
  {-# INLINE bifold #-}

  -- | Combines the elements of a structure, given ways of mapping them to a
  -- common monoid.
  --
  -- @'bifoldMap' f g ≡ 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'@
  bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
  bifoldMap f g = bifoldr (mappend . f) (mappend . g) mempty
  {-# INLINE bifoldMap #-}

  -- | Combines the elements of a structure in a right associative manner. Given
  -- a hypothetical function @toEitherList :: p a b -> [Either a b]@ yielding a
  -- list of all elements of a structure in order, the following would hold:
  --
  -- @'bifoldr' f g z ≡ 'foldr' ('either' f g) z . toEitherList@
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldr f g z t = appEndo (bifoldMap (Endo . f) (Endo . g) t) z
  {-# INLINE bifoldr #-}

  -- | Combines the elments of a structure in a left associative manner. Given a
  -- hypothetical function @toEitherList :: p a b -> [Either a b]@ yielding a
  -- list of all elements of a structure in order, the following would hold:
  --
  -- @'bifoldl' f g z ≡ 'foldl' (\acc -> 'either' (f acc) (g acc)) z .  toEitherList@
  bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
  bifoldl f g z t = appEndo (getDual (bifoldMap (Dual . Endo . flip f) (Dual . Endo . flip g) t)) z
  {-# INLINE bifoldl #-}

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL bifoldr | bifoldMap #-}
#endif

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
deriving instance Typeable Bifoldable
#endif

#if MIN_VERSION_semigroups(0,16,2)
instance Bifoldable Arg where
  bifoldMap f g (Arg a b) = f a `mappend` g b
#endif

instance Bifoldable (,) where
  bifoldMap f g ~(a, b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable Const where
  bifoldMap f _ (Const a) = f a
  {-# INLINE bifoldMap #-}

instance Bifoldable Constant where
  bifoldMap f _ (Constant a) = f a
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,) x) where
  bifoldMap f g ~(_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,) x y) where
  bifoldMap f g ~(_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,,) x y z) where
  bifoldMap f g ~(_,_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,,,) x y z w) where
  bifoldMap f g ~(_,_,_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

instance Bifoldable ((,,,,,,) x y z w v) where
  bifoldMap f g ~(_,_,_,_,_,a,b) = f a `mappend` g b
  {-# INLINE bifoldMap #-}

#ifdef MIN_VERSION_tagged
instance Bifoldable Tagged where
  bifoldMap _ g (Tagged b) = g b
  {-# INLINE bifoldMap #-}
#endif

instance Bifoldable Either where
  bifoldMap f _ (Left a) = f a
  bifoldMap _ g (Right b) = g b
  {-# INLINE bifoldMap #-}

-- | As 'bifoldr', but strict in the result of the reduction functions at each
-- step.
bifoldr' :: Bifoldable t => (a -> c -> c) -> (b -> c -> c) -> c -> t a b -> c
bifoldr' f g z0 xs = bifoldl f' g' id xs z0 where
  f' k x z = k $! f x z
  g' k x z = k $! g x z
{-# INLINE bifoldr' #-}

-- | Right associative monadic bifold over a structure.
bifoldrM :: (Bifoldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t a b -> m c
bifoldrM f g z0 xs = bifoldl f' g' return xs z0 where
  f' k x z = f x z >>= k
  g' k x z = g x z >>= k
{-# INLINE bifoldrM #-}

-- | As 'bifoldl', but strict in the result of the reductionf unctions at each
-- step.
bifoldl':: Bifoldable t => (a -> b -> a) -> (a -> c -> a) -> a -> t b c -> a
bifoldl' f g z0 xs = bifoldr f' g' id xs z0 where
  f' x k z = k $! f z x
  g' x k z = k $! g z x
{-# INLINE bifoldl' #-}

-- | Left associative monadic bifold over a structure.
bifoldlM :: (Bifoldable t, Monad m) => (a -> b -> m a) -> (a -> c -> m a) -> a -> t b c -> m a
bifoldlM f g z0 xs = bifoldr f' g' return xs z0 where
  f' x k z = f z x >>= k
  g' x k z = g z x >>= k
{-# INLINE bifoldlM #-}

-- | As 'Data.Bitraversable.bitraverse', but ignores the results of the
-- functions, merely performing the "actions".
bitraverse_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
bitraverse_ f g = bifoldr ((*>) . f) ((*>) . g) (pure ())
{-# INLINE bitraverse_ #-}

-- | As 'bitraverse_', but with the structure as the primary argument.
bifor_ :: (Bifoldable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f ()
bifor_ t f g = bitraverse_ f g t
{-# INLINE bifor_ #-}

-- | As 'Data.Bitraversable.bimapM', but ignores the results of the functions,
-- merely performing
-- the "actions".
bimapM_:: (Bifoldable t, Monad m) => (a -> m c) -> (b -> m d) -> t a b -> m ()
bimapM_ f g = bifoldr ((>>) . f) ((>>) . g) (return ())
{-# INLINE bimapM_ #-}

-- | As 'bimapM_', but with the structure as the primary argument.
biforM_ :: (Bifoldable t, Monad m) => t a b ->  (a -> m c) -> (b -> m d) -> m ()
biforM_ t f g = bimapM_ f g t
{-# INLINE biforM_ #-}

-- | As 'Data.Bitraversable.bisequenceA', but ignores the results of the actions.
bisequenceA_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
bisequenceA_ = bifoldr (*>) (*>) (pure ())
{-# INLINE bisequenceA_ #-}

-- | As 'Data.Bitraversable.bisequence', but ignores the results of the actions.
bisequence_ :: (Bifoldable t, Monad m) => t (m a) (m b) -> m ()
bisequence_ = bifoldr (>>) (>>) (return ())
{-# INLINE bisequence_ #-}

-- | Collects the list of elements of a structure in order.
biList :: Bifoldable t => t a a -> [a]
biList = bifoldr (:) (:) []
{-# INLINE biList #-}

-- | Reduces a structure of lists to the concatenation of those lists.
biconcat :: Bifoldable t => t [a] [a] -> [a]
biconcat = bifold
{-# INLINE biconcat #-}

-- | Given a means of mapping the elements of a structure to lists, computes the
-- concatenation of all such lists in order.
biconcatMap :: Bifoldable t => (a -> [c]) -> (b -> [c]) -> t a b -> [c]
biconcatMap = bifoldMap
{-# INLINE biconcatMap #-}

-- | Determines whether any element of the structure satisfies the appropriate
-- predicate.
biany :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biany p q = getAny . bifoldMap (Any . p) (Any . q)
{-# INLINE biany #-}

-- | Determines whether all elements of the structure satisfy the appropriate
-- predicate.
biall :: Bifoldable t => (a -> Bool) -> (b -> Bool) -> t a b -> Bool
biall p q = getAll . bifoldMap (All . p) (All . q)
{-# INLINE biall #-}
