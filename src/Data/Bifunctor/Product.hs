{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright   :  (C) 2008-2016 Jesse Selover, Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The product of two bifunctors.

module Data.Bifunctor.Product
( Product(..)
) where

import qualified Control.Arrow as A
import Control.Category
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Classes
import Data.Bifunctor.Functor
import Data.Bifunctor.Monoid
import Data.Bitraversable
import Data.Data
import Data.Functor.Classes
import Data.Functor.Contravariant
import GHC.Generics
import Prelude hiding ((.),id)

-- | Form the product of two bifunctors
data Product f g a b = Pair (f a b) (g a b)
  deriving ( Eq, Ord, Show, Read, Data, Generic, Generic1, Functor, Foldable, Traversable )

instance (Contravariant (f a), Contravariant (g a)) => Contravariant (Product f g a) where
  contramap = \f (Pair g h) -> Pair (contramap f g) (contramap f h)
  {-# inline contramap #-}

instance (Eq1 (f a), Eq1 (g a)) => Eq1 (Product f g a) where
  liftEq eq (Pair x1 y1) (Pair x2 y2) =
    liftEq eq x1 x2 && liftEq eq y1 y2
  {-# inline liftEq #-}

instance (Eq2 f, Eq2 g) => Eq2 (Product f g) where
  liftEq2 = \f g (Pair x1 y1) (Pair x2 y2) ->
    liftEq2 f g x1 x2 && liftEq2 f g y1 y2
  {-# inline liftEq2 #-}

instance (Ord1 (f a), Ord1 (g a)) => Ord1 (Product f g a) where
  liftCompare cmp (Pair x1 y1) (Pair x2 y2) =
    liftCompare cmp x1 x2 <> liftCompare cmp y1 y2
  {-# inline liftCompare #-}

instance (Ord2 f, Ord2 g) => Ord2 (Product f g) where
  liftCompare2 = \f g (Pair x1 y1) (Pair x2 y2) ->
    liftCompare2 f g x1 x2 `mappend` liftCompare2 f g y1 y2
  {-# inline liftCompare2 #-}

instance (Read2 f, Read2 g, Read a) => Read1 (Product f g a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Read2 f, Read2 g) => Read2 (Product f g) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 = readsData $
    readsBinaryWith (liftReadsPrec2 rp1 rl1 rp2 rl2)
                    (liftReadsPrec2 rp1 rl1 rp2 rl2)
                    "Pair" Pair

instance (Show2 f, Show2 g, Show a) => Show1 (Product f g a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show2 f, Show2 g) => Show2 (Product f g) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (Pair x y) =
    showsBinaryWith (liftShowsPrec2 sp1 sl1 sp2 sl2)
                    (liftShowsPrec2 sp1 sl1 sp2 sl2)
                    "Pair" p x y

instance (Bifunctor f, Bifunctor g) => Bifunctor (Product f g) where
  first = \f (Pair x y) -> Pair (first f x) (first f y)
  {-# inline first #-}
  second = \g (Pair x y) -> Pair (second g x) (second g y)
  {-# inline second #-}
  bimap = \f g (Pair x y) -> Pair (bimap f g x) (bimap f g y)
  {-# inline bimap #-}

instance (Biapplicative f, Biapplicative g) => Biapplicative (Product f g) where
  bipure = \a b -> Pair (bipure a b) (bipure a b)
  {-# inline bipure #-}
  biliftA2 = \f g (Pair w x) (Pair y z) -> Pair (biliftA2 f g w y) (biliftA2 f g x z)
  {-# inline biliftA2 #-}
  (<<*>>) = \(Pair w x) (Pair y z) -> Pair (w <<*>> y) (x <<*>> z)
  {-# inline (<<*>>) #-}

instance (Bifoldable f, Bifoldable g) => Bifoldable (Product f g) where
  bifoldMap = \f g (Pair x y) -> bifoldMap f g x `mappend` bifoldMap f g y
  {-# inline bifoldMap #-}

instance (Bitraversable f, Bitraversable g) => Bitraversable (Product f g) where
  bitraverse = \f g (Pair x y) -> Pair <$> bitraverse f g x <*> bitraverse f g y
  {-# inline bitraverse #-}

instance Bifunctor' p => BifunctorFunctor (Product p) where
  bifmap = \f (Pair p q) -> Pair p (f q)
  {-# inline bifmap #-}

instance Bifunctor' p => BifunctorComonad (Product p) where
  biextract = \(Pair _ q) -> q
  {-# inline biextract #-}
  biduplicate = \pq@(Pair p _) -> Pair p pq
  {-# inline biduplicate #-}
  biextend = \f pq@(Pair p _) -> Pair p (f pq)
  {-# inline biextend #-}

instance BifunctorMonoid p => BifunctorMonad (Product p) where
  bireturn = Pair mempty
  {-# inline bireturn #-}
  bijoin = \(Pair p (Pair q r)) -> Pair (p <> q) r
  {-# inline bijoin #-}

instance (Category p, Category q) => Category (Product p q) where
  id = Pair id id
  {-# inline id #-}
  (.) = \(Pair x y) (Pair x' y') -> Pair (x . x') (y . y')
  {-# inline (.) #-}

instance (A.Arrow p, A.Arrow q) => A.Arrow (Product p q) where
  arr = \f -> Pair (A.arr f) (A.arr f)
  {-# inline arr #-}
  first = \(Pair x y) -> Pair (A.first x) (A.first y)
  {-# inline first #-}
  second = \(Pair x y) -> Pair (A.second x) (A.second y)
  {-# inline second #-}
  (***) = \(Pair x y) (Pair x' y') -> Pair (x A.*** x') (y A.*** y')
  {-# inline (***) #-}
  (&&&) = \(Pair x y) (Pair x' y') -> Pair (x A.&&& x') (y A.&&& y')
  {-# inline (&&&) #-}

instance (A.ArrowChoice p, A.ArrowChoice q) => A.ArrowChoice (Product p q) where
  left = \(Pair x y) -> Pair (A.left x) (A.left y)
  {-# inline left #-}
  right = \(Pair x y) -> Pair (A.right x) (A.right y)
  {-# inline right #-}
  (+++) = \(Pair x y) (Pair x' y') -> Pair (x A.+++ x') (y A.+++ y')
  {-# inline (+++) #-}
  (|||) = \(Pair x y) (Pair x' y') -> Pair (x A.||| x') (y A.||| y')
  {-# inline (|||) #-}

instance (A.ArrowLoop p, A.ArrowLoop q) => A.ArrowLoop (Product p q) where
  loop = \(Pair x y) -> Pair (A.loop x) (A.loop y)
  {-# inline loop #-}

instance (A.ArrowZero p, A.ArrowZero q) => A.ArrowZero (Product p q) where
  zeroArrow = Pair A.zeroArrow A.zeroArrow
  {-# inline zeroArrow #-}

instance (A.ArrowPlus p, A.ArrowPlus q) => A.ArrowPlus (Product p q) where
  (<+>) = \(Pair x y) (Pair x' y') -> Pair (x A.<+> x') (y A.<+> y')
  {-# inline (<+>) #-}
