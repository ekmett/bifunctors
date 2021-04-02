{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Bifunctor.Functor
( (:->)
, BifunctorFunctor(..)
, BifunctorMonad(..)
, biliftM
, BifunctorComonad(..)
, biliftW
) where

#if __GLASGOW_HASKELL__ < 900
import Data.Bifunctor
#endif
import Data.Bifunctor.Classes

-- | Using parametricity as an approximation of a natural transformation in two arguments.
type (:->) p q = forall a b. p a b -> q a b
infixr 0 :->

class (forall a. Functor (f a)) => QFunctor f
instance (forall a. Functor (f a)) => QFunctor f

class
#if __GLASGOW_HASKELL__ < 900
  ( forall p. Bifunctor p => Bifunctor (t p)
  , forall p. (Bifunctor p, QFunctor p) => QFunctor (t p)
#else
  ( forall p. Bifunctor' p => Bifunctor' (t p)
#endif
  ) => BifunctorFunctor t where
-- class (forall p. Bifunctor' p => Bifunctor' (t p)) => BifunctorFunctor t where
  bifmap :: (p :-> q) -> t p :-> t q

class BifunctorFunctor t => BifunctorMonad t where
  bireturn :: Bifunctor' p => p :-> t p
  bibind   :: Bifunctor' q => (p :-> t q) -> t p :-> t q
  bibind f = bijoin . bifmap f
  bijoin   :: Bifunctor' p => t (t p) :-> t p
  bijoin = bibind id
  {-# MINIMAL bireturn, (bibind | bijoin) #-}

biliftM :: (BifunctorMonad t, Bifunctor' q) => (p :-> q) -> t p :-> t q
biliftM f = bibind (bireturn . f)
{-# INLINE biliftM #-}

class BifunctorFunctor t => BifunctorComonad t where
  biextract :: Bifunctor' p => t p :-> p
  biextend :: Bifunctor' p => (t p :-> q) -> t p :-> t q
  biextend f = bifmap f . biduplicate
  biduplicate :: Bifunctor' p => t p :-> t (t p)
  biduplicate =  biextend id
  {-# MINIMAL biextract, (biextend | biduplicate) #-}

biliftW :: (BifunctorComonad t, Bifunctor' p) => (p :-> q) -> t p :-> t q
biliftW f = biextend (f . biextract)
{-# INLINE biliftW #-}
