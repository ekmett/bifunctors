{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright   :  (C) 2021-2023 Edward Kmett and David Feuer
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :
--
-- 'Biapplicative's, backwards.

module Data.Biapplicative.Backwards
( Backwards (..)
) where

import Control.Applicative (Alternative)
import Data.Biapplicative
import Data.Bifunctor.ShowRead
import Data.Coerce
import qualified Data.Bifunctor as Base
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import GHC.Generics (Generic, Generic1)
import Data.Functor.Classes
import Control.Monad (MonadPlus)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Data.Functor.Contravariant (Contravariant)
import Data.Type.Equality (TestEquality)
import Data.Type.Coercion (TestCoercion)
import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift)

-- | An analogue of @"Control.Applicative.Backwards".'Control.Applicative.Backwards.Backwards'@
-- for bifunctors. The 'Biapplicative' instance performs actions
-- in the reverse order. All other instances are essentially derived ones.
--
-- @
-- 'bipure' a b = Backwards ('bipure' a b)
-- 'biliftA2' f g (Backwards m) (Backwards n) = Backwards $ 'biliftA2' ('flip' f) ('flip' g) n m
-- @
newtype Backwards p a b = Backwards { forwards :: p a b }
  deriving stock (Traversable, Generic, Generic1, Data, Lift)
  deriving newtype ( Eq, Ord, Functor, Foldable, Base.Bifunctor, Bifoldable
                   , Semigroup, Monoid, Applicative, Alternative, Monad, MonadFix
                   , MonadPlus, Fail.MonadFail, Contravariant, TestEquality, TestCoercion
                   , Eq1, Eq2, Ord1, Ord2 )

deriving via ShowRead (Backwards p a b) instance Show (p a b) => Show (Backwards p a b)

deriving via ShowRead1 (Backwards p a) instance Show1 (p a) => Show1 (Backwards p a)

deriving via ShowRead2 (Backwards p) instance Show2 p => Show2 (Backwards p)

-- | Accepts either plain or record syntax.
deriving via ShowRead (Backwards p a b) instance Read (p a b) => Read (Backwards p a b)

-- | Accepts either plain or record syntax.
deriving via ShowRead1 (Backwards p a) instance Read1 (p a) => Read1 (Backwards p a)

-- | Accepts either plain or record syntax.
deriving via ShowRead2 (Backwards p) instance Read2 p => Read2 (Backwards p)

instance Biapplicative p => Biapplicative (Backwards p) where
  bipure :: forall a b. a -> b -> Backwards p a b
  bipure = coerce (bipure @p @a @b)

  biliftA2 f g (Backwards m) (Backwards n) = Backwards $ biliftA2 (flip f) (flip g) n m

instance Bitraversable p => Bitraversable (Backwards p) where
  bitraverse f g (Backwards m) = Backwards <$> bitraverse f g m
