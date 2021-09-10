{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- From the Functional Pearl \"Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures\"
-- by Conor McBride.

module Data.Bifunctor.Joker
( Joker(..)
) where

import Control.Applicative (Applicative (..))
import Data.Bifunctor
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Unsafe
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Coerce
import Data.Data
import Data.Functor.Classes
import Data.Type.Equality (TestEquality)
import Data.Type.Coercion (TestCoercion)
import GHC.Generics

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Joker g a b = Joker { runJoker :: g b }
  deriving ( Eq, Ord, Data
           , Foldable, Traversable
           , Generic
           , Generic1
           )
  deriving newtype (TestEquality, TestCoercion)

instance Eq1 g => Eq1 (Joker g a) where
  liftEq = eqJoker #. liftEq
  {-# inline liftEq #-}

instance Eq1 g => Eq2 (Joker g) where
  liftEq2 = \_ -> eqJoker #. liftEq
  {-# inline liftEq2 #-}

instance Ord1 g => Ord1 (Joker g a) where
  liftCompare = compareJoker #. liftCompare
  {-# inline liftCompare #-}

instance Ord1 g => Ord2 (Joker g) where
  liftCompare2 _ = compareJoker #. liftCompare
  {-# inline liftCompare2 #-}

deriving via ShowRead (Joker g a b) instance Show (g b) => Show (Joker g a b)

deriving via ShowRead1 (Joker g a) instance Show1 g => Show1 (Joker g a)

-- | Accepts either plain or record syntax.
deriving via ShowRead (Joker g a b) instance Read (g b) => Read (Joker g a b)

-- | Accepts either plain or record syntax.
deriving via ShowRead1 (Joker g a) instance Read1 g => Read1 (Joker g a)

instance Read1 g => Read2 (Joker g) where
  liftReadPrec2 _ _ rp2 rl2 = liftReadPrecWhatever $ liftReadPrec rp2 rl2
  liftReadListPrec2 = liftReadListPrec2Default

instance Show1 g => Show2 (Joker g) where
  liftShowsPrec2 _ _ sp2 sl2 = liftShowsPrecWhatever $ liftShowsPrec sp2 sl2

eqJoker :: (g b1 -> g b2 -> Bool)
        -> Joker g a1 b1 -> Joker g a2 b2 -> Bool
eqJoker = coerce
{-# inline eqJoker #-}

compareJoker :: (g b1 -> g b2 -> Ordering)
             -> Joker g a1 b1 -> Joker g a2 b2 -> Ordering
compareJoker = coerce
{-# inline compareJoker #-}

instance Functor g => Bifunctor (Joker g) where
  first _ = Joker #. runJoker
  {-# inline first #-}
  second g = Joker #. fmap g .# runJoker
  {-# inline second #-}
  bimap _ g = Joker #. fmap g .# runJoker
  {-# inline bimap #-}

instance Functor g => Functor (Joker g a) where
  fmap g = Joker #. fmap g .# runJoker
  {-# inline fmap #-}

instance Applicative g => Biapplicative (Joker g) where
  bipure _ = Joker #. pure
  {-# inline bipure #-}

  (<<*>>) :: forall a b c d. Joker g (a -> b) (c -> d) -> Joker g a c -> Joker g b d
  (<<*>>) = coerce ((<*>) :: g (c -> d) -> g c -> g d)
  {-# inline (<<*>>) #-}

  biliftA2 :: forall a b c d e f. (a -> b -> c) -> (d -> e -> f) -> Joker g a d -> Joker g b e -> Joker g c f
  biliftA2 _ = coerce (liftA2 :: (d -> e -> f) -> g d -> g e -> g f)

instance Foldable g => Bifoldable (Joker g) where
  bifoldMap _ g = foldMap g .# runJoker
  {-# inline bifoldMap #-}

  bifoldr _c1 c2 n = foldr c2 n .# runJoker
  {-# inline bifoldr #-}

  bifoldl _c1 c2 n = foldl c2 n .# runJoker
  {-# inline bifoldl #-}

instance Traversable g => Bitraversable (Joker g) where
  bitraverse _ g = fmap Joker . traverse g .# runJoker
  {-# inline bitraverse #-}
