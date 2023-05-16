{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright   :  (C) 2008-2023 Edward Kmett
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable

module Data.Bifunctor.Fix
( Fix(..)
) where

import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.ShowRead
import Data.Bitraversable
import Data.Data
import Data.Functor.Classes
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)

-- | Greatest fixpoint of a 'Bifunctor' (a 'Functor' over the first argument with zipping).
newtype Fix p a = In { out :: p (Fix p a) a }
  deriving (Generic)

deriving instance Eq   (p (Fix p a) a) => Eq   (Fix p a)
deriving instance Ord  (p (Fix p a) a) => Ord  (Fix p a)
deriving instance Lift (p (Fix p a) a) => Lift (Fix p a)
deriving via ShowRead (Fix p a) instance Show (p (Fix p a) a) => Show (Fix p a)
deriving via ShowRead (Fix p a) instance Read (p (Fix p a) a) => Read (Fix p a)

deriving instance
  ( Typeable k, Typeable p, Typeable a
  , Data (p (Fix p a) a)
  ) => Data (Fix p (a :: k))

instance Eq2 p => Eq1 (Fix p) where
  liftEq f (In x) (In y) = liftEq2 (liftEq f) f x y

instance Ord2 p => Ord1 (Fix p) where
  liftCompare f (In x) (In y) = liftCompare2 (liftCompare f) f x y

instance Read2 p => Read1 (Fix p) where
  liftReadPrec rp rl = go
    where
      go = liftReadPrecWhatever $ liftReadPrec2 go (liftReadListPrec rp rl) rp rl
  liftReadListPrec = liftReadListPrecDefault

instance Show2 p => Show1 (Fix p) where
  liftShowsPrec sp1 sl1 = go
    where
      go = liftShowsPrecWhatever (liftShowsPrec2 go (liftShowList sp1 sl1) sp1 sl1)

instance Bifunctor p => Functor (Fix p) where
  fmap f (In p) = In (bimap (fmap f) f p)
  {-# INLINE fmap #-}

instance Biapplicative p => Applicative (Fix p) where
  pure a = In (bipure (pure a) a)
  {-# INLINE pure #-}
  In p <*> In q = In (biliftA2 (<*>) ($) p q)
  {-# INLINE (<*>) #-}

instance Bifoldable p => Foldable (Fix p) where
  foldMap f (In p) = bifoldMap (foldMap f) f p
  {-# INLINE foldMap #-}

instance Bitraversable p => Traversable (Fix p) where
  traverse f (In p) = In <$> bitraverse (traverse f) f p
  {-# INLINE traverse #-}
