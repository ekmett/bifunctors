{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- From the Functional Pearl \"Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures\"
-- by Conor McBride.

module Data.Bifunctor.Clown
( Clown(..)
) where

import Data.Coerce
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifoldable1 (Bifoldable1(..))
import Data.Bifunctor
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Unsafe
import Data.Bitraversable
import Data.Foldable1 (Foldable1(..))
import Data.Functor.Contravariant
import Data.Data
import Data.Functor.Classes
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import Text.Read (Read (..), readListPrecDefault)

-- | Make a 'Functor' over the first argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Clown f a b = Clown { runClown :: f a }
  deriving (Eq, Ord, Data, Generic, Generic1, Lift)

instance Eq (f a) => Eq1 (Clown f a) where
  liftEq _eq = eqClown (==)
  {-# inline liftEq #-}

instance Eq1 f => Eq2 (Clown f) where
  liftEq2 = \f _ -> eqClown (liftEq f)
  {-# inline liftEq2 #-}

instance Ord (f a) => Ord1 (Clown f a) where
  liftCompare _cmp = compareClown compare
  {-# inline liftCompare #-}

instance Ord1 f => Ord2 (Clown f) where
  liftCompare2 = \f _ -> compareClown (liftCompare f)
  {-# inline liftCompare2 #-}

instance Read (f a) => Read (Clown f a b) where
  readPrec = liftReadPrecWhatever readPrec
  readListPrec = readListPrecDefault

instance Show (f a) => Show (Clown f a b) where
  showsPrec = liftShowsPrecWhatever showsPrec

instance Read (f a) => Read1 (Clown f a) where
  liftReadPrec _ _ = liftReadPrecWhatever readPrec
  liftReadListPrec = liftReadListPrecDefault

instance Show (f a) => Show1 (Clown f a) where
  liftShowsPrec _ _ = liftShowsPrecWhatever showsPrec

instance Read1 f => Read2 (Clown f) where
  liftReadPrec2 rp rl _ _ = liftReadPrecWhatever (liftReadPrec rp rl)
  liftReadListPrec2 = liftReadListPrec2Default

instance Show1 f => Show2 (Clown f) where
  liftShowsPrec2 sp sl _ _ = liftShowsPrecWhatever (liftShowsPrec sp sl)

eqClown :: (f a1 -> f a2 -> Bool)
        -> Clown f a1 b1 -> Clown f a2 b2 -> Bool
eqClown = coerce

compareClown :: (f a1 -> f a2 -> Ordering)
             -> Clown f a1 b1 -> Clown f a2 b2 -> Ordering
compareClown = coerce

instance Functor f => Bifunctor (Clown f) where
  first = \f -> Clown #. fmap f .# runClown
  {-# INLINE first #-}
  second = \_ -> Clown #. runClown
  {-# INLINE second #-}
  bimap = \f _ -> Clown #. fmap f .# runClown
  {-# INLINE bimap #-}

instance Functor (Clown f a) where
  fmap _ = coerce
  {-# inline fmap #-}

instance Contravariant (Clown f a) where
  contramap _ = coerce
  {-# inline contramap #-}

instance Applicative f => Biapplicative (Clown f) where
  bipure a _ = Clown (pure a)
  {-# INLINE bipure #-}

  (<<*>>) = \mf -> Clown #. (<*>) (runClown mf) .# runClown
  {-# INLINE (<<*>>) #-}

instance Foldable f => Bifoldable (Clown f) where
  bifoldMap f _ = foldMap f .# runClown
  {-# INLINE bifoldMap #-}
  bifoldr c1 _c2 n = foldr c1 n .# runClown
  {-# INLINE bifoldr #-}
  bifoldl c1 _c2 n = foldl c1 n .# runClown
  {-# INLINE bifoldl #-}

instance Foldable1 f => Bifoldable1 (Clown f) where
  bifoldMap1 f _ = foldMap1 f . runClown
  {-# INLINE bifoldMap1 #-}

instance Foldable (Clown f a) where
  foldMap _ = mempty
  {-# INLINE foldMap #-}

instance Traversable f => Bitraversable (Clown f) where
  bitraverse f _ = fmap Clown . traverse f .# runClown
  {-# INLINE bitraverse #-}

instance Traversable (Clown f a) where
  traverse _ = pure .# coerce
  {-# INLINE traverse #-}
