{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DataKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

module Data.Bifunctor.Uncurry
    ( Uncurry (Uncurry)
    )
where

import Data.Ix
import Data.Semigroup

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable (Typeable)
#endif

import Foreign.Ptr (castPtr)
import Foreign.Storable

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

#if __GLASGOW_HASKELL__ >= 704
#define Pair(a, b) '(a, b)
#else
#define Pair(a, b) (a, b)
#endif

data Uncurry f p where
    -- This is strict because Uncurry is morally a newtype
    Uncurry :: !(f a b) -> Uncurry f (Pair(a, b))
  deriving
    (
#if __GLASGOW_HASKELL__ >= 708
      Typeable
#endif
    )
deriving instance (Eq (f a b)) => Eq (Uncurry f (Pair(a, b)))
deriving instance (Ord (f a b)) => Ord (Uncurry f (Pair(a, b)))
deriving instance (Read (f a b)) => Read (Uncurry f (Pair(a, b)))
deriving instance (Show (f a b)) => Show (Uncurry f (Pair(a, b)))

instance Bounded (f a b) => Bounded (Uncurry f (Pair(a, b))) where
    minBound = Uncurry minBound
    maxBound = Uncurry maxBound

instance Enum (f a b) => Enum (Uncurry f (Pair(a, b))) where
    toEnum = Uncurry . toEnum
    fromEnum (Uncurry f) = fromEnum f

instance Ix (f a b) => Ix (Uncurry f (Pair(a, b))) where
    range (Uncurry a, Uncurry b) = map Uncurry (range (a, b))
    index (Uncurry a, Uncurry b) (Uncurry i) = index (a, b) i
    inRange (Uncurry a, Uncurry b) (Uncurry i) = inRange (a, b) i

instance Semigroup (f a b) => Semigroup (Uncurry f (Pair(a, b))) where
    Uncurry a <> Uncurry b = Uncurry (a <> b)

instance Monoid (f a b) => Monoid (Uncurry f (Pair(a, b))) where
    mempty = Uncurry mempty
    mappend (Uncurry a) (Uncurry b) = Uncurry (mappend a b)

instance Storable (f a b) => Storable (Uncurry f (Pair(a, b))) where
    sizeOf (_ :: Uncurry f Pair(a, b)) = sizeOf (undefined :: f a b)
    alignment (_ :: Uncurry f Pair(a, b)) = alignment (undefined :: f a b)
    peek = fmap Uncurry . peek . castPtr
    poke ptr (Uncurry a) = poke (castPtr ptr) a

#if __GLASGOW_HASKELL__ >= 702
#if __GLASGOW_HASKELL__ >= 711
type UncurryMetaData = MetaData "Uncurry" "Data.Bifunctor.Uncurry" "main" False
type UncurryMetaCons = MetaCons "Uncurry" PrefixI 'False
type UncurryMetaSel = MetaSel Nothing NoSourceUnpackedness SourceStrict DecidedStrict
#else
data UncurryMetaData
data UncurryMetaCons
type UncurryMetaSel = NoSelector
instance Datatype UncurryMetaData where
    datatypeName _ = "Uncurry"
    moduleName _ = "Data.Bifunctor.Uncurry"

instance Constructor UncurryMetaCons where
    conName _ = "Uncurry"
#endif

instance Generic (Uncurry f Pair(a, b)) where
    type Rep (Uncurry f Pair(a, b)) = D1 UncurryMetaData
        (C1 UncurryMetaCons (S1 UncurryMetaSel (Rec0 (f a b))))
    from (Uncurry f) = M1 (M1 (M1 (K1 f)))
    to (M1 (M1 (M1 (K1 f)))) = Uncurry f
#endif
