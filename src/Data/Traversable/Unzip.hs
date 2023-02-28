{-# LANGUAGE Trustworthy #-}

module Data.Traversable.Unzip
  ( unzipWith
  ) where

import Data.Biapplicative
import Data.Bifunctor.Unsafe
import qualified Data.Bifunctor as Base

newtype LazyPair a b = LazyPair { unLazyPair :: (a, b) }

-- Getting the thunks we want when we want them is quite fragile. I found
-- it helpful to inspect the Core for unzipping Maps; those have enough
-- strictness to make GHC want to do bad things that will leak memory.
instance Functor (LazyPair a) where
  fmap f (LazyPair ab) = combine a b
    where
      ~(a, b) = ab
      combine x y = LazyPair (x, f y)
      {-# NOINLINE combine #-}
instance Base.Bifunctor LazyPair where
  bimap f g (LazyPair ab) = combine a b
    where
      ~(a, b) = ab
      combine p q = LazyPair (f p, g q)
      {-# NOINLINE combine #-}
instance Biapplicative LazyPair where
  bipure x y = LazyPair (x, y)
  biliftA2 f g (LazyPair x1y1) (LazyPair x2y2) = combine x1 x2 y1 y2
    where
      ~(x1, y1) = x1y1
      ~(x2, y2) = x2y2
      -- I worked out this "combine" trick for Data.List.transpose.
      -- By marking the combine function NOINLINE, we ensure that
      -- all four selector thunks are constructed up front. In particular,
      -- we don't let GHC do something like
      --
      -- (let {~(x1, _) = x1y1; ~(x2, _) = x2y2} in f x1 x2,
      --  let {~(_, y1) = x1y1; ~(_, y2) = x2y2} in g y1 y2)
      combine p q r s = LazyPair (f p q, g r s)
      {-# NOINLINE combine #-}

-- | A lazy unzipping function designed to avoid leaking memory.
--
-- @
-- unzipWith f as =
--   let bcs = fmap f as
--   in as `seq` (fmap fst bcs, fmap snd bcs)
-- @
--
-- Unlike the naive implementation, @unzipWith@ does not retain pair components
-- unnecessarily. Consider @unzipWith id abs@, where the first component of
-- each element of @abs@ is large. If you walk the first component of the
-- result, forcing and consuming all its elements, then none of those will be
-- retained by the second component of the result.
--
-- Caution: while we have done our best to avoid memory leaks, we cannot
-- absolutely guarantee that they will not occur.
unzipWith :: Traversable t => (a -> (b, c)) -> t a -> (t b, t c)
unzipWith f = unLazyPair . traverseBia (LazyPair #. f)
{-# INLINABLE unzipWith #-}
