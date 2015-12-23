{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

module Data.Bifunctor.Sum where

import Data.Bifunctor
import Data.Bifunctor.Functor
import Data.Bifoldable
import Data.Bitraversable
#if __GLASGOW_HASKELL__ < 710
import Data.Functor
#endif
import Data.Semigroup hiding (Sum)
#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

data Sum p q a b = L2 (p a b) | R2 (q a b)
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Generic1
           , Typeable
#endif
           )

instance (Bounded (p a b), Bounded (q a b)) => Bounded (Sum p q a b) where
    minBound = L2 minBound
    maxBound = R2 maxBound

instance Semigroup (Sum p q a b) where
    L2 _ <> b = b
    a <> _ = a

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 708
data SumMetaData
data SumMetaConsL2
data SumMetaConsR2

instance Datatype SumMetaData where
    datatypeName _ = "Sum"
    moduleName _ = "Data.Bifunctor.Sum"

instance Constructor SumMetaConsL2 where
    conName _ = "L2"

instance Constructor SumMetaConsR2 where
    conName _ = "R2"

instance Generic1 (Sum p q a) where
    type Rep1 (Sum p q a) = D1 SumMetaData ((:+:)
        (C1 SumMetaConsL2 (S1 NoSelector (Rec1 (p a))))
        (C1 SumMetaConsR2 (S1 NoSelector (Rec1 (q a)))))
    from1 (L2 p) = M1 (L1 (M1 (M1 (Rec1 p))))
    from1 (R2 q) = M1 (R1 (M1 (M1 (Rec1 q))))
    to1 (M1 (L1 (M1 (M1 p)))) = L2 (unRec1 p)
    to1 (M1 (R1 (M1 (M1 q)))) = R2 (unRec1 q)
#endif

instance (Bifunctor p, Bifunctor q) => Bifunctor (Sum p q) where
  bimap f g (L2 p) = L2 (bimap f g p)
  bimap f g (R2 q) = R2 (bimap f g q)
  first f (L2 p) = L2 (first f p)
  first f (R2 q) = R2 (first f q)
  second f (L2 p) = L2 (second f p)
  second f (R2 q) = R2 (second f q)

instance (Bifoldable p, Bifoldable q) => Bifoldable (Sum p q) where
  bifoldMap f g (L2 p) = bifoldMap f g p
  bifoldMap f g (R2 q) = bifoldMap f g q

instance (Bitraversable p, Bitraversable q) => Bitraversable (Sum p q) where
  bitraverse f g (L2 p) = L2 <$> bitraverse f g p
  bitraverse f g (R2 q) = R2 <$> bitraverse f g q

instance BifunctorFunctor (Sum p) where
  bifmap _ (L2 p) = L2 p
  bifmap f (R2 q) = R2 (f q)

instance BifunctorMonad (Sum p) where
  bireturn = R2
  bijoin (L2 p) = L2 p
  bijoin (R2 q) = q
  bibind _ (L2 p) = L2 p
  bibind f (R2 q) = f q
