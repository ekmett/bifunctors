{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Join
  ( Join(..)
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

-- | Make a 'Functor' over both arguments of a 'Bifunctor'.
newtype Join p a = Join { runJoin :: p a a }
#if __GLASGOW_HASKELL__ >= 708
  deriving Typeable
#endif

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

instance Bifoldable p => Foldable (Join p) where
  foldMap f (Join a) = bifoldMap f f a
  {-# INLINE foldMap #-}

instance Bitraversable p => Traversable (Join p) where
  traverse f (Join a) = fmap Join (bitraverse f f a)
  {-# INLINE traverse #-}
  sequenceA (Join a) = fmap Join (bisequenceA a)
  {-# INLINE sequenceA #-}
