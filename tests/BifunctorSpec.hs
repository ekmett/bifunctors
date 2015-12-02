{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-|
Module:      BifunctorSpec
Copyright:   (C) 2008-2015 Edward Kmett, (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Edward Kmett
Portability: Template Haskell

@hspec@ tests for the "Data.Bifunctor.TH" module.
-}
module BifunctorSpec where

import Data.Bifunctor
import Data.Bifunctor.TH
import Data.Bifoldable
import Data.Bitraversable

import Data.Char (chr)
import Data.Functor.Classes (Eq1)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative(..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

-------------------------------------------------------------------------------

-- Adapted from the test cases from
-- https://ghc.haskell.org/trac/ghc/attachment/ticket/2953/deriving-functor-tests.patch

-- Plain data types

data Strange a b c
    = T1 a b c
    | T2 [a] [b] [c]         -- lists
    | T3 [[a]] [[b]] [[c]]   -- nested lists
    | T4 (c,(b,b),(c,c))     -- tuples
    | T5 ([c],Strange a b c) -- tycons

type IntFun a b = (b -> Int) -> a
data StrangeFunctions a b c
    = T6 (a -> c)            -- function types
    | T7 (a -> (c,a))        -- functions and tuples
    | T8 ((b -> a) -> c)     -- continuation
    | T9 (IntFun b c)        -- type synonyms

data StrangeGADT a b where
    T10 :: Ord b            => b        -> StrangeGADT a b
    T11 ::                     Int      -> StrangeGADT a Int
    T12 :: c ~ Int          => c        -> StrangeGADT a Int
    T13 :: b ~ Int          => Int      -> StrangeGADT a b
    T14 :: b ~ Int          => b        -> StrangeGADT a b
    T15 :: (b ~ c, c ~ Int) => Int -> c -> StrangeGADT a b

data NotPrimitivelyRecursive a b
    = S1 (NotPrimitivelyRecursive (a,a) (b, a))
    | S2 a
    | S3 b

newtype OneTwoCompose f g a b = OneTwoCompose (f (g a b))
  deriving (Arbitrary, Eq, Show)

newtype ComplexConstraint f g a b = ComplexConstraint (f Int Int (g a,a,b))

data Universal a b
    = Universal  (forall b. (b,[a]))
    | Universal2 (forall f. Bifunctor f => f a b)
    | Universal3 (forall a. Maybe a) -- reuse a
    | NotReallyUniversal (forall b. a)

data Existential a b
    = forall a. ExistentialList [a]
    | forall f. Bitraversable f => ExistentialFunctor (f a b)
    | forall b. SneakyUseSameName (Maybe b)

-- Data families

data family   StrangeFam a  b c
data instance StrangeFam a  b c
    = T1Fam a b c
    | T2Fam [a] [b] [c]         -- lists
    | T3Fam [[a]] [[b]] [[c]]   -- nested lists
    | T4Fam (c,(b,b),(c,c))     -- tuples
    | T5Fam ([c],Strange a b c) -- tycons

data family   StrangeFunctionsFam a b c
data instance StrangeFunctionsFam a b c
    = T6Fam (a -> c)            -- function types
    | T7Fam (a -> (c,a))        -- functions and tuples
    | T8Fam ((b -> a) -> c)     -- continuation
    | T9Fam (IntFun b c)        -- type synonyms

data family   StrangeGADTFam a b
data instance StrangeGADTFam a b where
    T10Fam :: Ord b            => b        -> StrangeGADTFam a b
    T11Fam ::                     Int      -> StrangeGADTFam a Int
    T12Fam :: c ~ Int          => c        -> StrangeGADTFam a Int
    T13Fam :: b ~ Int          => Int      -> StrangeGADTFam a b
    T14Fam :: b ~ Int          => b        -> StrangeGADTFam a b
    T15Fam :: (b ~ c, c ~ Int) => Int -> c -> StrangeGADTFam a b

data family   NotPrimitivelyRecursiveFam a b
data instance NotPrimitivelyRecursiveFam a b
    = S1Fam (NotPrimitivelyRecursive (a,a) (b, a))
    | S2Fam a
    | S3Fam b

data family      OneTwoComposeFam (f :: * -> *) (g :: * -> * -> *) a b
newtype instance OneTwoComposeFam f g a b = OneTwoComposeFam (f (g a b))
  deriving (Arbitrary, Eq, Show)

data family      ComplexConstraintFam (f :: * -> * -> * -> *) (g :: * -> *) a b
newtype instance ComplexConstraintFam f g a b = ComplexConstraintFam (f Int Int (g a,a,b))

data family   UniversalFam a b
data instance UniversalFam a b
    = UniversalFam  (forall b. (b,[a]))
    | Universal2Fam (forall f. Bifunctor f => f a b)
    | Universal3Fam (forall a. Maybe a) -- reuse a
    | NotReallyUniversalFam (forall b. a)

data family   ExistentialFam a b
data instance ExistentialFam a b
    = forall a. ExistentialListFam [a]
    | forall f. Bitraversable f => ExistentialFunctorFam (f a b)
    | forall b. SneakyUseSameNameFam (Maybe b)

-------------------------------------------------------------------------------

-- Plain data types

$(deriveBifunctor     ''Strange)
$(deriveBifoldable    ''Strange)
$(deriveBitraversable ''Strange)

$(deriveBifunctor     ''StrangeFunctions)
$(deriveBifoldable    ''StrangeGADT)

$(deriveBifunctor     ''NotPrimitivelyRecursive)
$(deriveBifoldable    ''NotPrimitivelyRecursive)
$(deriveBitraversable ''NotPrimitivelyRecursive)

$(deriveBifunctor     ''OneTwoCompose)
$(deriveBifoldable    ''OneTwoCompose)
$(deriveBitraversable ''OneTwoCompose)

instance (Bifunctor (f Int), Functor g) =>
  Bifunctor (ComplexConstraint f g) where
    bimap = $(makeBimap ''ComplexConstraint)
instance (Bifoldable (f Int), Foldable g) =>
  Bifoldable (ComplexConstraint f g) where
    bifoldr   = $(makeBifoldr ''ComplexConstraint)
    bifoldMap = $(makeBifoldMap ''ComplexConstraint)
instance (Bitraversable (f Int), Traversable g) =>
  Bitraversable (ComplexConstraint f g) where
    bitraverse = $(makeBitraverse ''ComplexConstraint)

$(deriveBifunctor     ''Universal)

$(deriveBifunctor     ''Existential)
$(deriveBifoldable    ''Existential)
$(deriveBitraversable ''Existential)

#if MIN_VERSION_template_haskell(2,7,0)
-- Data families

$(deriveBifunctor     'T1Fam)
$(deriveBifoldable    'T2Fam)
$(deriveBitraversable 'T3Fam)

$(deriveBifunctor     'T6Fam)
$(deriveBifoldable    'T10Fam)

$(deriveBifunctor     'S1Fam)
$(deriveBifoldable    'S2Fam)
$(deriveBitraversable 'S3Fam)

$(deriveBifunctor     'OneTwoComposeFam)
$(deriveBifoldable    'OneTwoComposeFam)
$(deriveBitraversable 'OneTwoComposeFam)

instance (Bifunctor (f Int), Functor g) =>
  Bifunctor (ComplexConstraintFam f g) where
    bimap = $(makeBimap 'ComplexConstraintFam)
instance (Bifoldable (f Int), Foldable g) =>
  Bifoldable (ComplexConstraintFam f g) where
    bifoldr   = $(makeBifoldr 'ComplexConstraintFam)
    bifoldMap = $(makeBifoldMap 'ComplexConstraintFam)
instance (Bitraversable (f Int), Traversable g) =>
  Bitraversable (ComplexConstraintFam f g) where
    bitraverse = $(makeBitraverse 'ComplexConstraintFam)

$(deriveBifunctor     'UniversalFam)

$(deriveBifunctor     'ExistentialListFam)
$(deriveBifoldable    'ExistentialFunctorFam)
$(deriveBitraversable 'SneakyUseSameNameFam)
#endif

-------------------------------------------------------------------------------

prop_BifunctorLaws :: (Bifunctor p, Eq (p a b), Eq (p c d))
                   => (a -> c) -> (b -> d) -> p a b -> Bool
prop_BifunctorLaws f g x =
       bimap  id id x == x
    && first  id    x == x
    && second id    x == x
    && bimap  f  g  x == (first f . second g) x

prop_BifunctorEx :: (Bifunctor p, Eq (p [Int] [Int])) => p [Int] [Int] -> Bool
prop_BifunctorEx = prop_BifunctorLaws reverse (++ [42])

prop_BifoldableLaws :: (Eq a, Eq b, Eq z, Monoid a, Monoid b, Bifoldable p)
                => (a -> b) -> (a -> b)
                -> (a -> z -> z) -> (a -> z -> z)
                -> z -> p a a -> Bool
prop_BifoldableLaws f g h i z x =
       bifold        x == bifoldMap id id x
    && bifoldMap f g x == bifoldr (mappend . f) (mappend . g) mempty x
    && bifoldr h i z x == appEndo (bifoldMap (Endo . h) (Endo . i) x) z

prop_BifoldableEx :: Bifoldable p => p [Int] [Int] -> Bool
prop_BifoldableEx = prop_BifoldableLaws reverse (++ [42]) ((+) . length) ((*) . length) 0

prop_BitraversableLaws :: (Applicative f, Bitraversable p, Eq (f (p c c)),
                           Eq (p a b), Eq (p d e), Eq1 f)
                       => (a -> f c) -> (b -> f c) -> (c -> f d) -> (c -> f e)
                       -> (f c -> f c) -> p a b -> Bool
prop_BitraversableLaws f g h i t x =
       bitraverse (t . f) (t . g)   x == bitraverse f g x
    && bitraverse Identity Identity x == Identity x
    && (Compose . fmap (bitraverse h i) . bitraverse f g) x
       == bitraverse (Compose . fmap h . f) (Compose . fmap i . g) x

prop_BitraversableEx :: (Bitraversable p, Eq (p Char Char),
                        Eq (p [Char] [Char]), Eq (p [Int] [Int]))
                        => p [Int] [Int] -> Bool
prop_BitraversableEx = prop_BitraversableLaws
    (replicate 2 . map (chr . abs))
    (replicate 4 . map (chr . abs))
    (++ "hello")
    (++ "world")
    reverse

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "OneTwoCompose Maybe Either [Int] [Int]" $ do
        prop "satisfies the Bifunctor laws"
            (prop_BifunctorEx     :: OneTwoCompose Maybe Either [Int] [Int] -> Bool)
        prop "satisfies the Bifoldable laws"
            (prop_BifoldableEx    :: OneTwoCompose Maybe Either [Int] [Int] -> Bool)
        prop "satisfies the Bitraversable laws"
            (prop_BitraversableEx :: OneTwoCompose Maybe Either [Int] [Int] -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)
    describe "OneTwoComposeFam Maybe Either [Int] [Int]" $ do
        prop "satisfies the Bifunctor laws"
            (prop_BifunctorEx     :: OneTwoComposeFam Maybe Either [Int] [Int] -> Bool)
        prop "satisfies the Bifoldable laws"
            (prop_BifoldableEx    :: OneTwoComposeFam Maybe Either [Int] [Int] -> Bool)
        prop "satisfies the Bitraversable laws"
            (prop_BitraversableEx :: OneTwoComposeFam Maybe Either [Int] [Int] -> Bool)
#endif
