{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- From the Functional Pearl \"Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures\"
-- by Conor McBride.
----------------------------------------------------------------------------
module Data.Bifunctor.Clown
  ( Clown(..)
  ) where

import Data.Biapplicative
import Data.Bifoldable
import Data.Bifoldable1 (Bifoldable1(..))
import Data.Bitraversable
import Data.Foldable1 (Foldable1(..))
import Data.Functor.Classes
import GHC.Generics

-- | Make a 'Functor' over the first argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Clown f a b = Clown { runClown :: f a }
  deriving (Eq, Ord, Show, Read, Generic, Generic1)

instance (Eq1 f, Eq a) => Eq1 (Clown f a) where
  liftEq = liftEq2 (==)
instance Eq1 f => Eq2 (Clown f) where
  liftEq2 f _ = eqClown (liftEq f)

instance (Ord1 f, Ord a) => Ord1 (Clown f a) where
  liftCompare = liftCompare2 compare
instance Ord1 f => Ord2 (Clown f) where
  liftCompare2 f _ = compareClown (liftCompare f)

instance (Read1 f, Read a) => Read1 (Clown f a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
instance Read1 f => Read2 (Clown f) where
  liftReadsPrec2 rp1 rl1 _ _ = readsPrecClown (liftReadsPrec rp1 rl1)

instance (Show1 f, Show a) => Show1 (Clown f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Show1 f => Show2 (Clown f) where
  liftShowsPrec2 sp1 sl1 _ _ = showsPrecClown (liftShowsPrec sp1 sl1)

eqClown :: (f a1 -> f a2 -> Bool)
        -> Clown f a1 b1 -> Clown f a2 b2 -> Bool
eqClown eqA (Clown x) (Clown y) = eqA x y

compareClown :: (f a1 -> f a2 -> Ordering)
             -> Clown f a1 b1 -> Clown f a2 b2 -> Ordering
compareClown compareA (Clown x) (Clown y) = compareA x y

readsPrecClown :: (Int -> ReadS (f a))
               -> Int -> ReadS (Clown f a b)
readsPrecClown rpA p =
  readParen (p > 10) $ \s0 -> do
    ("Clown",    s1) <- lex s0
    ("{",        s2) <- lex s1
    ("runClown", s3) <- lex s2
    (x,          s4) <- rpA 0 s3
    ("}",        s5) <- lex s4
    return (Clown x, s5)

showsPrecClown :: (Int -> f a -> ShowS)
               -> Int -> Clown f a b -> ShowS
showsPrecClown spA p (Clown x) =
  showParen (p > 10) $
      showString "Clown {runClown = "
    . spA 0 x
    . showChar '}'

instance Functor f => Bifunctor (Clown f) where
  first f = Clown . fmap f . runClown
  {-# INLINE first #-}
  second _ = Clown . runClown
  {-# INLINE second #-}
  bimap f _ = Clown . fmap f . runClown
  {-# INLINE bimap #-}

instance Functor (Clown f a) where
  fmap _ = Clown . runClown
  {-# INLINE fmap #-}

instance Applicative f => Biapplicative (Clown f) where
  bipure a _ = Clown (pure a)
  {-# INLINE bipure #-}

  Clown mf <<*>> Clown mx = Clown (mf <*> mx)
  {-# INLINE (<<*>>) #-}

instance Foldable f => Bifoldable (Clown f) where
  bifoldMap f _ = foldMap f . runClown
  {-# INLINE bifoldMap #-}

instance Foldable1 f => Bifoldable1 (Clown f) where
  bifoldMap1 f _ = foldMap1 f . runClown
  {-# INLINE bifoldMap1 #-}

instance Foldable (Clown f a) where
  foldMap _ = mempty
  {-# INLINE foldMap #-}

instance Traversable f => Bitraversable (Clown f) where
  bitraverse f _ = fmap Clown . traverse f . runClown
  {-# INLINE bitraverse #-}

instance Traversable (Clown f a) where
  traverse _ = pure . Clown . runClown
  {-# INLINE traverse #-}
