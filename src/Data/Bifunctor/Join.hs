{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   :  (C) 2008-2023 Edward Kmett
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable

module Data.Bifunctor.Join
( Join(..)
) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (Applicative (liftA2))
#endif
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifoldable1 (Bifoldable1(..))
import Data.Bifunctor
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Unsafe
import Data.Bitraversable
import Data.Coerce
import Data.Data
import Data.Foldable1 (Foldable1(..))
import Data.Functor.Classes
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import Text.Read (Read (..), readListPrecDefault)

-- | Make a 'Functor' over both arguments of a 'Bifunctor'.
newtype Join p a = Join { runJoin :: p a a }
  deriving Generic

deriving instance Eq   (p a a) => Eq   (Join p a)
deriving instance Ord  (p a a) => Ord  (Join p a)
deriving instance
  ( Typeable k, Typeable p, Typeable a, Data (p a a)
  ) => Data (Join p (a :: k))
deriving stock instance Lift (p a a) => Lift (Join p a)

instance Eq2 p => Eq1 (Join p) where
  liftEq :: forall a b. (a -> b -> Bool) -> Join p a -> Join p b -> Bool
  liftEq f = coerce (liftEq2 f f :: p a a -> p b b -> Bool)

instance Ord2 p => Ord1 (Join p) where
  liftCompare :: forall a b. (a -> b -> Ordering) -> Join p a -> Join p b -> Ordering
  liftCompare f = coerce (liftCompare2 f f :: p a a -> p b b -> Ordering)

instance Read (p a a) => Read (Join p a) where
  readPrec = liftReadPrecWhatever readPrec
  readListPrec = readListPrecDefault

instance Read2 p => Read1 (Join p) where
  liftReadPrec rp rl = liftReadPrecWhatever $ liftReadPrec2 rp rl rp rl
  liftReadListPrec = liftReadListPrecDefault

instance Show (p a a) => Show (Join p a) where
  showsPrec = liftShowsPrecWhatever showsPrec

instance Show2 p => Show1 (Join p) where
  liftShowsPrec sp1 sl1 = liftShowsPrecWhatever $ liftShowsPrec2 sp1 sl1 sp1 sl1

mapJoin :: (p a a -> p b b) -> Join p a -> Join p b
mapJoin = coerce

mapJoin2 :: (p a a -> p b b -> p c c) -> Join p a -> Join p b -> Join p c
mapJoin2 = coerce

instance Bifunctor p => Functor (Join p) where
  fmap :: forall a b. (a -> b) -> Join p a -> Join p b
  fmap f = mapJoin (bimap f f)
  {-# inline fmap #-}

instance Biapplicative p => Applicative (Join p) where
  pure a = Join $ bipure a a
  {-# inline pure #-}
  liftA2 = \f -> mapJoin2 (biliftA2 f f)
  {-# inline liftA2 #-}
  (<*>) = mapJoin2 (<<*>>)
  {-# inline (<*>) #-}
  (*>) = mapJoin2 (*>>)
  {-# inline (*>) #-}
  (<*) = mapJoin2 (<<*)
  {-# inline (<*) #-}

instance Bifoldable p => Foldable (Join p) where
  foldMap f = bifoldMap f f .# runJoin
  {-# inline foldMap #-}

instance Bifoldable1 p => Foldable1 (Join p) where
  foldMap1 f (Join a) = bifoldMap1 f f a
  {-# INLINE foldMap1 #-}

instance Bitraversable p => Traversable (Join p) where
  traverse f = fmap Join . bitraverse f f .# runJoin
  {-# inline traverse #-}
  sequenceA = fmap Join . bisequenceA .# runJoin
  {-# inline sequenceA #-}
