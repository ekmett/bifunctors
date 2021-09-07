{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright   :  (C) 2021 David Feuer
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :
--
-- Making bifunctors whose elements are notionally in the
-- reverse order from the original bifunctor.

module Data.Bifunctor.Reverse
  ( Reverse (..)
  ) where

import Control.Applicative (Alternative)
import Data.Biapplicative
import Data.Bifunctor.ShowRead
import qualified Data.Bifunctor as Base
import GHC.Generics (Generic, Generic1)
import qualified Data.Functor.Reverse as FunReverse
import Control.Applicative.Backwards
import Data.Coerce
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Semigroup (Dual (..))
import Data.Functor.Classes
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Data.Functor.Contravariant (Contravariant)
import Data.Type.Equality (TestEquality)
import Data.Type.Coercion (TestCoercion)
import Data.Data (Data)

-- | The same bifunctor, but with `Bifoldable`, `Bitraversable`,
-- `Foldable` and `Traversable` instances that process the elements
-- in the reverse order. All other instances are essentially derived
-- ones.
--
-- @
-- 'bitraverse'
--   (\c -> do print c; (,) c <$> (readLn :: IO Int))
--   (\b -> do print b; pure b)
--   (Reverse $ "Data.Bifunctor.Tannen".'Data.Bifunctor.Tannen.Tannen' [Left 'a', Right False, Left 'q'])
--
-- 'q' -- output
-- 12  -- input
-- False -- output
-- 'a' -- output
-- 13 -- input
-- Reverse ('Data.Bifunctor.Tannen.Tannen' {runTannen = [Left ('a',13),Right False,Left ('q',12)]}) -- output
-- @
newtype Reverse t a b = Reverse { getReverse :: t a b }
  deriving stock (Generic, Generic1, Data)
  deriving Foldable via FunReverse.Reverse (t a)

  deriving newtype ( Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix
                   , Fail.MonadFail, Contravariant, TestEquality, TestCoercion
                   , Eq, Eq1, Eq2, Ord, Ord1, Ord2
                   , Base.Bifunctor, Biapplicative, Semigroup, Monoid )

instance Bifoldable t => Bifoldable (Reverse t) where
  bifoldMap f g (Reverse t) = getDual $ bifoldMap (coerce f) (coerce g) t
  bifoldr c1 c2 n (Reverse t) = bifoldl (flip c1) (flip c2) n t
  bifoldl c1 c2 b (Reverse t) = bifoldr (flip c1) (flip c2) b t
  -- We can't do anything special for bifold.

instance Bitraversable t => Bitraversable (Reverse t) where
  bitraverse f g (Reverse t) = fmap Reverse . forwards $ bitraverse (coerce f) (coerce g) t

instance Traversable (t a) => Traversable (Reverse t a) where
  traverse f (Reverse t) = fmap Reverse . forwards $ traverse (coerce f) t

deriving via ShowRead (Reverse p a b) instance Show (p a b) => Show (Reverse p a b)

deriving via ShowRead1 (Reverse p a) instance Show1 (p a) => Show1 (Reverse p a)

deriving via ShowRead2 (Reverse p) instance Show2 p => Show2 (Reverse p)

-- | Accepts either plain or record syntax.
deriving via ShowRead (Reverse p a b) instance Read (p a b) => Read (Reverse p a b)

-- | Accepts either plain or record syntax.
deriving via ShowRead1 (Reverse p a) instance Read1 (p a) => Read1 (Reverse p a)

-- | Accepts either plain or record syntax.
deriving via ShowRead2 (Reverse p) instance Read2 p => Read2 (Reverse p)
