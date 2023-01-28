{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

module Data.Bifunctor.Functor
  ( (:->)
  , BifunctorFunctor(..)
  , BifunctorMonad(..)
  , biliftM
  , BifunctorComonad(..)
  , biliftW
  ) where

-- | Using parametricity as an approximation of a natural transformation in two arguments.
type (:->) p q = forall a b. p a b -> q a b
infixr 0 :->

class BifunctorFunctor t where
  bifmap :: (p :-> q) -> t p :-> t q

class BifunctorFunctor t => BifunctorMonad t where
  bireturn :: p :-> t p
  bibind   :: (p :-> t q) -> t p :-> t q
  bibind f = bijoin . bifmap f
  bijoin   :: t (t p) :-> t p
  bijoin = bibind id
  {-# MINIMAL bireturn, (bibind | bijoin) #-}

biliftM :: BifunctorMonad t => (p :-> q) -> t p :-> t q
biliftM f = bibind (bireturn . f)
{-# INLINE biliftM #-}

class BifunctorFunctor t => BifunctorComonad t where
  biextract :: t p :-> p
  biextend :: (t p :-> q) -> t p :-> t q
  biextend f = bifmap f . biduplicate
  biduplicate :: t p :-> t (t p)
  biduplicate =  biextend id
  {-# MINIMAL biextract, (biextend | biduplicate) #-}

biliftW :: BifunctorComonad t => (p :-> q) -> t p :-> t q
biliftW f = biextend (f . biextract)
{-# INLINE biliftW #-}
