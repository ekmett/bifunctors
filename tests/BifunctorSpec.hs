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

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

prop_BifunctorLaws :: (Bifunctor p, Eq (p a b), Eq (p c d))
                   => (a -> c) -> (b -> d) -> p a b -> Bool
prop_BifunctorLaws f g x =
       bimap  id id x == x
    && first  id    x == x
    && second id    x == x
    && bimap  f  g  x == (first f . second g) x

prop_BifoldableLaws :: (Eq a, Eq b, Eq z, Monoid a, Monoid b, Bifoldable p)
                => (a -> b) -> (a -> b)
                -> (a -> z -> z) -> (a -> z -> z)
                -> z -> p a a -> Bool
prop_BifoldableLaws f g h i z x =
       bifold        x == bifoldMap id id x
    && bifoldMap f g x == bifoldr (mappend . f) (mappend . g) mempty x
    && bifoldr h i z x == appEndo (bifoldMap (Endo . h) (Endo . i) x) z

prop_BitraversableLaws :: (Applicative f, Bitraversable p, Eq (f (p c c)),
                           Eq (p a b), Eq (p d e), Eq1 f)
                       => (a -> f c) -> (b -> f c) -> (c -> f d) -> (c -> f e)
                       -> (f c -> f c) -> p a b -> Bool
prop_BitraversableLaws f g h i t x =
       bitraverse (t . f) (t . g)   x == bitraverse f g x
    && bitraverse Identity Identity x == Identity x
    && (Compose . fmap (bitraverse h i) . bitraverse f g) x
       == bitraverse (Compose . fmap h . f) (Compose . fmap i . g) x

-------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "OneTwoCompose Maybe Either [Int] [Int]" $ do
        prop "satisfies the Bifunctor laws"
            (prop_BifunctorLaws
                reverse
                (++ [42])
                :: OneTwoCompose Maybe Either [Int] [Int] -> Bool)
        prop "satisfies the Bifoldable laws"
            (prop_BifoldableLaws
                reverse (++ [42])
                ((+) . length)
                ((*) . length)
                0
                :: OneTwoCompose Maybe Either [Int] [Int] -> Bool)
        prop "satisfies the Bitraversable laws"
            (prop_BitraversableLaws
                (replicate 2 . map (chr . abs))
                (replicate 4 . map (chr . abs))
                ((++ "hello"))
                ((++ "world"))
                reverse
                :: OneTwoCompose Maybe Either [Int] [Int] -> Bool)
