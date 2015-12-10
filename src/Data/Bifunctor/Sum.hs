{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Bifunctor.Sum where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
#if __GLASGOW_HASKELL__ < 710
import Data.Functor
#endif
import Data.Typeable

data Sum p q a b = L2 (p a b) | R2 (q a b)
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 708
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
