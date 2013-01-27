-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bitraversable
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bitraversable
  ( Bitraversable(..)
  , bifor
  , biforM
  , bimapAccumL
  , bimapAccumR
  , bimapDefault
  , bifoldMapDefault
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Monoid
import Data.Tagged

class (Bifunctor t, Bifoldable t) => Bitraversable t where
  bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
  bitraverse f g = bisequenceA . bimap f g
  {-# INLINE bitraverse #-}

  bisequenceA :: Applicative f => t (f a) (f b) -> f (t a b)
  bisequenceA = bitraverse id id
  {-# INLINE bisequenceA #-}

  bimapM :: Monad m => (a -> m c) -> (b -> m d) -> t a b -> m (t c d)
  bimapM f g = unwrapMonad . bitraverse (WrapMonad . f) (WrapMonad . g)
  {-# INLINE bimapM #-}

  bisequence :: Monad m => t (m a) (m b) -> m (t a b)
  bisequence = bimapM id id
  {-# INLINE bisequence #-}

instance Bitraversable (,) where
  bitraverse f g ~(a, b) = (,) <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,) x) where
  bitraverse f g ~(x, a, b) = (,,) x <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,,) x y) where
  bitraverse f g ~(x, y, a, b) = (,,,) x y <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable ((,,,,) x y z) where
  bitraverse f g ~(x, y, z, a, b) = (,,,,) x y z <$> f a <*> g b
  {-# INLINE bitraverse #-}

instance Bitraversable Either where
  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b
  {-# INLINE bitraverse #-}

instance Bitraversable Const where
  bitraverse f _ (Const a) = Const <$> f a
  {-# INLINE bitraverse #-}

instance Bitraversable Tagged where
  bitraverse _ g (Tagged b) = Tagged <$> g b
  {-# INLINE bitraverse #-}

bifor :: (Bitraversable t, Applicative f) => t a b -> (a -> f c) -> (b -> f d) -> f (t c d)
bifor t f g = bitraverse f g t
{-# INLINE bifor #-}

biforM :: (Bitraversable t, Monad m) =>  t a b -> (a -> m c) -> (b -> m d) -> m (t c d)
biforM t f g = bimapM f g t
{-# INLINE biforM #-}

-- | left-to-right state transformer
newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
  fmap f (StateL k) = StateL $ \ s ->
    let (s', v) = k s in (s', f v)
  {-# INLINE fmap #-}

instance Applicative (StateL s) where
  pure x = StateL (\ s -> (s, x))
  {-# INLINE pure #-}
  StateL kf <*> StateL kv = StateL $ \ s ->
    let (s', f) = kf s
        (s'', v) = kv s'
    in (s'', f v)
  {-# INLINE (<*>) #-}

bimapAccumL :: Bitraversable t => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumL f g s t = runStateL (bitraverse (StateL . flip f) (StateL . flip g) t) s
{-# INLINE bimapAccumL #-}

-- | right-to-left state transformer
newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where
  fmap f (StateR k) = StateR $ \ s ->
    let (s', v) = k s in (s', f v)
  {-# INLINE fmap #-}

instance Applicative (StateR s) where
  pure x = StateR (\ s -> (s, x))
  {-# INLINE pure #-}
  StateR kf <*> StateR kv = StateR $ \ s ->
    let (s', v) = kv s
        (s'', f) = kf s'
    in (s'', f v)
  {-# INLINE (<*>) #-}

bimapAccumR :: Bitraversable t => (a -> b -> (a, c)) -> (a -> d -> (a, e)) -> a -> t b d -> (a, t c e)
bimapAccumR f g s t = runStateR (bitraverse (StateR . flip f) (StateR . flip g) t) s
{-# INLINE bimapAccumR #-}

newtype Id a = Id { getId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)
  {-# INLINE fmap #-}

instance Applicative Id where
  pure = Id
  {-# INLINE pure #-}
  Id f <*> Id x = Id (f x)
  {-# INLINE (<*>) #-}

bimapDefault :: Bitraversable t => (a -> b) -> (c -> d) -> t a c -> t b d
bimapDefault f g = getId . bitraverse (Id . f) (Id . g)
{-# INLINE bimapDefault #-}

bifoldMapDefault :: (Bitraversable t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
bifoldMapDefault f g = getConst . bitraverse (Const . f) (Const . g)
{-# INLINE bifoldMapDefault #-}
