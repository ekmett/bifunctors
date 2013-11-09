{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Join
-- Copyright   :  (C) 2008-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Join
  ( Join(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Apply
import Data.Bitraversable
import Data.Foldable
import Data.Functor.Bind
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable

-- | Make a 'Functor' over both arguments of a 'Bifunctor'.
newtype Join p a = Join { runJoin :: p a a }

deriving instance Eq   (p a a) => Eq   (Join p a)
deriving instance Ord  (p a a) => Ord  (Join p a)
deriving instance Show (p a a) => Show (Join p a)
deriving instance Read (p a a) => Read (Join p a)

instance Bifunctor p => Functor (Join p) where
  fmap f (Join a) = Join (bimap f f a)
  {-# INLINE fmap #-}

instance Biapplicative p => Applicative (Join p) where
  pure a = Join (bipure a a)
  {-# INLINE pure #-}
  Join f <*> Join a = Join (f <<*>> a)
  {-# INLINE (<*>) #-}
  Join a *> Join b = Join (a *>> b)
  {-# INLINE (*>) #-}
  Join a <* Join b = Join (a <<* b)
  {-# INLINE (<*) #-}

instance Biapply p => Apply (Join p) where
  Join f <.> Join a = Join (f <<.>> a)
  {-# INLINE (<.>) #-}
  Join a .> Join b = Join (a .>> b)
  {-# INLINE (.>) #-}
  Join a <. Join b = Join (a <<. b)
  {-# INLINE (<.) #-}

instance Bifoldable p => Foldable (Join p) where
  foldMap f (Join a) = bifoldMap f f a
  {-# INLINE foldMap #-}

instance Bitraversable p => Traversable (Join p) where
  traverse f (Join a) = fmap Join (bitraverse f f a)
  {-# INLINE traverse #-}
  sequenceA (Join a) = fmap Join (bisequenceA a)
  {-# INLINE sequenceA #-}

instance Bifoldable1 p => Foldable1 (Join p) where
  foldMap1 f (Join a) = bifoldMap1 f f a
  {-# INLINE foldMap1 #-}

instance Bitraversable1 p => Traversable1 (Join p) where
  traverse1 f (Join a) = fmap Join (bitraverse1 f f a)
  {-# INLINE traverse1 #-}
  sequence1 (Join a) = fmap Join (bisequence1 a)
  {-# INLINE sequence1 #-}
