{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
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
import GHC.Generics (Generic, Generic1)
import Language.Haskell.TH.Syntax (Lift)
import Text.Read ((+++))

data Sum p q a b
  = L2 (p a b)
  | R2 (q a b)
  deriving stock
    ( Eq, Ord, Show, Read, Data, Generic, Generic1
    , Functor, Foldable, Traversable, Lift )

instance (Eq1 (p a), Eq1 (q a)) => Eq1 (Sum p q a) where
  liftEq eq (L2 x) (L2 y) = liftEq eq x y
  liftEq _ (L2 _) (R2 _) = False
  liftEq _ (R2 _) (L2 _) = False
  liftEq eq (R2 x) (R2 y) = liftEq eq x y
  {-# inline liftEq #-}

instance (Eq2 f, Eq2 g) => Eq2 (Sum f g) where
  liftEq2 f g (L2 x1) (L2 x2) = liftEq2 f g x1 x2
  liftEq2 _ _ (L2 _)  (R2 _)  = False
  liftEq2 _ _ (R2 _)  (L2 _)  = False
  liftEq2 f g (R2 y1) (R2 y2) = liftEq2 f g y1 y2
  {-# inline liftEq2 #-}

instance (Ord1 (p a), Ord1 (q a)) => Ord1 (Sum p q a) where
  liftCompare cmp (L2 x) (L2 y) = liftCompare cmp x y
  liftCompare cmp (R2 x) (R2 y) = liftCompare cmp x y
  liftCompare _ (L2 _) (R2 _) = LT
  liftCompare _ (R2 _) (L2 _) = GT
  {-# inline liftCompare #-}

instance (Ord2 f, Ord2 g) => Ord2 (Sum f g) where
  liftCompare2 f g (L2 x1) (L2 x2) = liftCompare2 f g x1 x2
  liftCompare2 _ _ (L2 _)  (R2 _)  = LT
  liftCompare2 _ _ (R2 _)  (L2 _)  = GT
  liftCompare2 f g (R2 y1) (R2 y2) = liftCompare2 f g y1 y2
  {-# inline liftCompare2 #-}

instance (Read1 (f a), Read1 (g a)) => Read1 (Sum f g a) where
  liftReadPrec rp rl = readData $
    readUnaryWith (liftReadPrec rp rl) "L2" L2 +++
    readUnaryWith (liftReadPrec rp rl) "R2" R2

instance (Read2 f, Read2 g) => Read2 (Sum f g) where
  liftReadPrec2 rp1 rl1 rp2 rl2 = readData $
    readUnaryWith (liftReadPrec2 rp1 rl1 rp2 rl2) "L2" L2 +++
    readUnaryWith (liftReadPrec2 rp1 rl1 rp2 rl2) "R2" R2

instance (Show1 (f a), Show1 (g a)) => Show1 (Sum f g a) where
  liftShowsPrec sp sl p (L2 x) =
    showsUnaryWith (liftShowsPrec sp sl) "L2" p x
  liftShowsPrec sp sl p (R2 y) =
    showsUnaryWith (liftShowsPrec sp sl) "R2" p y

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
