{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DeriveDataTypeable #-}
#endif

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
