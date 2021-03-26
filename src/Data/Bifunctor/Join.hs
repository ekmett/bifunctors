{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable

module Data.Bifunctor.Join
( Join(..)
) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Unsafe
import Data.Bitraversable
import Data.Coerce
import Data.Data
import Data.Functor.Classes
import GHC.Generics

-- | Make a 'Functor' over both arguments of a 'Bifunctor'.
newtype Join p a = Join { runJoin :: p a a }
  deriving Generic

deriving instance Eq   (p a a) => Eq   (Join p a)
deriving instance Ord  (p a a) => Ord  (Join p a)
deriving instance Show (p a a) => Show (Join p a)
deriving instance Read (p a a) => Read (Join p a)
deriving instance
  ( Typeable k, Typeable p, Typeable a, Data (p a a)
  ) => Data (Join p (a :: k))

instance Eq2 p => Eq1 (Join p) where
  liftEq :: forall a b. (a -> b -> Bool) -> Join p a -> Join p b -> Bool
  liftEq f = coerce (liftEq2 f f :: p a a -> p b b -> Bool)

instance Ord2 p => Ord1 (Join p) where
  liftCompare :: forall a b. (a -> b -> Ordering) -> Join p a -> Join p b -> Ordering
  liftCompare f = coerce (liftCompare2 f f :: p a a -> p b b -> Ordering)

instance Read2 p => Read1 (Join p) where
  liftReadsPrec rp1 rl1 p = readParen (p > 10) $ \s0 -> do
    ("Join",    s1) <- lex s0
    ("{",       s2) <- lex s1
    ("runJoin", s3) <- lex s2
    (x,         s4) <- liftReadsPrec2 rp1 rl1 rp1 rl1 0 s3
    ("}",       s5) <- lex s4
    return (Join x, s5)

instance Show2 p => Show1 (Join p) where
  liftShowsPrec sp1 sl1 p (Join x) = showParen (p > 10) $
      showString "Join {runJoin = "
    . liftShowsPrec2 sp1 sl1 sp1 sl1 0 x
    . showChar '}'

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

instance Bitraversable p => Traversable (Join p) where
  traverse f = fmap Join . bitraverse f f .# runJoin
  {-# inline traverse #-}
  sequenceA = fmap Join . bisequenceA .# runJoin
  {-# inline sequenceA #-}
