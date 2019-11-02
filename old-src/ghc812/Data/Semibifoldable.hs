{-# LANGUAGE CPP, TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Semibifoldable where

import Control.Applicative
import Data.Bifoldable

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

import Data.Semigroup as Semigroup hiding (Product, Sum)
import Data.Orphans ()
-- import Data.Ord -- missing Foldable, https://ghc.haskell.org/trac/ghc/ticket/15098#ticket

import Prelude hiding (foldr)

-- TODO: move Semibifoldable to bifunctors renaming to Semibifoldable or Bisemifoldable?
class Bifoldable t => Semibifoldable t where
  semibifold :: Semigroup m => t m m -> m
  semibifold = semibifoldMap id id
  {-# INLINE semibifold #-}

  semibifoldMap :: Semigroup m => (a -> m) -> (b -> m) -> t a b -> m
  semibifoldMap f g = maybe (error "semibifoldMap") id . getOption . bifoldMap (Option . Just . f) (Option . Just . g)
  {-# INLINE semibifoldMap #-}

instance Semibifoldable Arg where
  semibifoldMap f g (Arg a b) = f a <> g b

instance Semibifoldable Either where
  semibifoldMap f _ (Left a) = f a
  semibifoldMap _ g (Right b) = g b
  {-# INLINE semibifoldMap #-}

instance Semibifoldable (,) where
  semibifoldMap f g (a, b) = f a <> g b
  {-# INLINE semibifoldMap #-}

instance Semibifoldable ((,,) x) where
  semibifoldMap f g (_,a,b) = f a <> g b
  {-# INLINE semibifoldMap #-}

instance Semibifoldable ((,,,) x y) where
  semibifoldMap f g (_,_,a,b) = f a <> g b
  {-# INLINE semibifoldMap #-}

instance Semibifoldable ((,,,,) x y z) where
  semibifoldMap f g (_,_,_,a,b) = f a <> g b
  {-# INLINE semibifoldMap #-}

instance Semibifoldable Const where
  semibifoldMap f _ (Const a) = f a
  {-# INLINE semibifoldMap #-}

#ifdef MIN_VERSION_tagged
instance Semibifoldable Tagged where
  semibifoldMap _ g (Tagged b) = g b
  {-# INLINE semibifoldMap #-}
#endif
