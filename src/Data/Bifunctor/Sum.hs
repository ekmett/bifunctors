{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}

module Data.Bifunctor.Sum 
( Sum(..)
) where

import Data.Bifunctor
import Data.Bifunctor.Classes
import Data.Bifunctor.Functor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import Data.Functor.Classes
import GHC.Generics

data Sum p q a b
  = L2 (p a b)
  | R2 (q a b)
  deriving (Eq, Ord, Show, Read, Data, Generic, Generic1, Functor, Foldable, Traversable)

instance (Eq2 f, Eq2 g, Eq a) => Eq1 (Sum f g a) where
  liftEq = liftEq2 (==)
  {-# inline liftEq #-}

instance (Eq2 f, Eq2 g) => Eq2 (Sum f g) where
  liftEq2 f g (L2 x1) (L2 x2) = liftEq2 f g x1 x2
  liftEq2 _ _ (L2 _)  (R2 _)  = False
  liftEq2 _ _ (R2 _)  (L2 _)  = False
  liftEq2 f g (R2 y1) (R2 y2) = liftEq2 f g y1 y2
  {-# inline liftEq2 #-}

instance (Ord2 f, Ord2 g, Ord a) => Ord1 (Sum f g a) where
  liftCompare = liftCompare2 compare
  {-# inline liftCompare #-}

instance (Ord2 f, Ord2 g) => Ord2 (Sum f g) where
  liftCompare2 f g (L2 x1) (L2 x2) = liftCompare2 f g x1 x2
  liftCompare2 _ _ (L2 _)  (R2 _)  = LT
  liftCompare2 _ _ (R2 _)  (L2 _)  = GT
  liftCompare2 f g (R2 y1) (R2 y2) = liftCompare2 f g y1 y2
  {-# inline liftCompare2 #-}

instance (Read2 f, Read2 g, Read a) => Read1 (Sum f g a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Read2 f, Read2 g) => Read2 (Sum f g) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 = readsData $
    readsUnaryWith (liftReadsPrec2 rp1 rl1 rp2 rl2) "L2" L2 `mappend`
    readsUnaryWith (liftReadsPrec2 rp1 rl1 rp2 rl2) "R2" R2

instance (Show2 f, Show2 g, Show a) => Show1 (Sum f g a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show2 f, Show2 g) => Show2 (Sum f g) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (L2 x) =
    showsUnaryWith (liftShowsPrec2 sp1 sl1 sp2 sl2) "L2" p x
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (R2 y) =
    showsUnaryWith (liftShowsPrec2 sp1 sl1 sp2 sl2) "R2" p y

instance (Bifunctor p, Bifunctor q) => Bifunctor (Sum p q) where
  bimap = \f g -> \case
    L2 p -> L2 (bimap f g p)
    R2 q -> R2 (bimap f g q)
  {-# inline bimap #-}
  first = \f -> \case
    L2 p -> L2 (first f p)
    R2 q -> R2 (first f q)
  {-# inline first #-}
  second = \f -> \case
    L2 p -> L2 (second f p)
    R2 q -> R2 (second f q)
  {-# inline second #-}

instance (Bifoldable p, Bifoldable q) => Bifoldable (Sum p q) where
  bifoldMap = \f g -> \case
    L2 p -> bifoldMap f g p
    R2 q -> bifoldMap f g q
  {-# inline bifoldMap #-}

instance (Bitraversable p, Bitraversable q) => Bitraversable (Sum p q) where
  bitraverse = \f g -> \case
    L2 p -> L2 <$> bitraverse f g p
    R2 q -> R2 <$> bitraverse f g q
  {-# inline bitraverse #-}

instance Bifunctor' p => BifunctorFunctor (Sum p) where
  bifmap = \f -> \case
    L2 p -> L2 p
    R2 q -> R2 (f q)
  {-# inline bifmap #-}

instance Bifunctor' p => BifunctorMonad (Sum p) where
  bireturn = R2
  {-# inline bireturn #-}
  bijoin = \case
    L2 p -> L2 p
    R2 q -> q
  {-# inline bijoin #-}
  bibind = \f -> \case
    L2 p -> L2 p
    R2 q -> f q
  {-# inline bibind #-}
