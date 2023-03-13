{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Tannen
  ( Tannen(..)
  ) where

import Control.Applicative

import Control.Arrow as A
import Control.Category
import Control.Comonad

import Data.Bifunctor as B
import Data.Bifunctor.Functor
import Data.Bifunctor.Swap (Swap (..))
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifoldable1 (Bifoldable1(..))
import Data.Bitraversable
import Data.Foldable1 (Foldable1(..))
import Data.Functor.Classes

import GHC.Generics

import Prelude hiding ((.),id)

-- | Compose a 'Functor' on the outside of a 'Bifunctor'.
newtype Tannen f p a b = Tannen { runTannen :: f (p a b) }
  deriving (Eq, Ord, Show, Read, Generic)
deriving instance Functor f => Generic1 (Tannen f p a)

instance (Eq1 f, Eq2 p, Eq a) => Eq1 (Tannen f p a) where
  liftEq = liftEq2 (==)
instance (Eq1 f, Eq2 p) => Eq2 (Tannen f p) where
  liftEq2 f g (Tannen x) (Tannen y) = liftEq (liftEq2 f g) x y

instance (Ord1 f, Ord2 p, Ord a) => Ord1 (Tannen f p a) where
  liftCompare = liftCompare2 compare
instance (Ord1 f, Ord2 p) => Ord2 (Tannen f p) where
  liftCompare2 f g (Tannen x) (Tannen y) = liftCompare (liftCompare2 f g) x y

instance (Read1 f, Read2 p, Read a) => Read1 (Tannen f p a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Read1 f, Read2 p) => Read2 (Tannen f p) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 p = readParen (p > 10) $ \s0 -> do
    ("Tannen",    s1) <- lex s0
    ("{",         s2) <- lex s1
    ("runTannen", s3) <- lex s2
    (x,           s4) <- liftReadsPrec (liftReadsPrec2 rp1 rl1 rp2 rl2)
                                       (liftReadList2  rp1 rl1 rp2 rl2) 0 s3
    ("}",         s5) <- lex s4
    return (Tannen x, s5)

instance (Show1 f, Show2 p, Show a) => Show1 (Tannen f p a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance (Show1 f, Show2 p) => Show2 (Tannen f p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (Tannen x) = showParen (p > 10) $
      showString "Tannen {runTannen = "
    . liftShowsPrec (liftShowsPrec2 sp1 sl1 sp2 sl2)
                    (liftShowList2  sp1 sl1 sp2 sl2) 0 x
    . showChar '}'

instance Functor f => BifunctorFunctor (Tannen f) where
  bifmap f (Tannen fp) = Tannen (fmap f fp)

instance (Functor f, Monad f) => BifunctorMonad (Tannen f) where
  bireturn = Tannen . return
  bibind f (Tannen fp) = Tannen $ fp >>= runTannen . f

instance Comonad f => BifunctorComonad (Tannen f) where
  biextract = extract . runTannen
  biextend f (Tannen fp) = Tannen (extend (f . Tannen) fp)

instance (Functor f, Bifunctor p) => Bifunctor (Tannen f p) where
  first f = Tannen . fmap (B.first f) . runTannen
  {-# INLINE first #-}
  second f = Tannen . fmap (B.second f) . runTannen
  {-# INLINE second #-}
  bimap f g = Tannen . fmap (bimap f g) . runTannen
  {-# INLINE bimap #-}

instance (Functor f, Bifunctor p) => Functor (Tannen f p a) where
  fmap f = Tannen . fmap (B.second f) . runTannen
  {-# INLINE fmap #-}

instance (Applicative f, Biapplicative p) => Biapplicative (Tannen f p) where
  bipure a b = Tannen (pure (bipure a b))
  {-# INLINE bipure #-}

  Tannen fg <<*>> Tannen xy = Tannen ((<<*>>) <$> fg <*> xy)
  {-# INLINE (<<*>>) #-}

instance (Foldable f, Bifoldable p) => Foldable (Tannen f p a) where
  foldMap f = foldMap (bifoldMap (const mempty) f) . runTannen
  {-# INLINE foldMap #-}

instance (Foldable f, Bifoldable p) => Bifoldable (Tannen f p) where
  bifoldMap f g = foldMap (bifoldMap f g) . runTannen
  {-# INLINE bifoldMap #-}

instance (Foldable1 f, Bifoldable1 p) => Bifoldable1 (Tannen f p) where
  bifoldMap1 f g = foldMap1 (bifoldMap1 f g) . runTannen
  {-# INLINE bifoldMap1 #-}

instance (Traversable f, Bitraversable p) => Traversable (Tannen f p a) where
  traverse f = fmap Tannen . traverse (bitraverse pure f) . runTannen
  {-# INLINE traverse #-}

instance (Traversable f, Bitraversable p) => Bitraversable (Tannen f p) where
  bitraverse f g = fmap Tannen . traverse (bitraverse f g) . runTannen
  {-# INLINE bitraverse #-}

instance (Applicative f, Category p) => Category (Tannen f p) where
  id = Tannen $ pure id
  Tannen fpbc . Tannen fpab = Tannen $ liftA2 (.) fpbc fpab

instance (Applicative f, Arrow p) => Arrow (Tannen f p) where
  arr f = Tannen $ pure $ arr f
  first = Tannen . fmap A.first . runTannen
  second = Tannen . fmap A.second . runTannen
  Tannen ab *** Tannen cd = Tannen $ liftA2 (***) ab cd
  Tannen ab &&& Tannen ac = Tannen $ liftA2 (&&&) ab ac

instance (Applicative f, ArrowChoice p) => ArrowChoice (Tannen f p) where
  left  = Tannen . fmap left . runTannen
  right = Tannen . fmap right . runTannen
  Tannen ab +++ Tannen cd = Tannen $ liftA2 (+++) ab cd
  Tannen ac ||| Tannen bc = Tannen $ liftA2 (|||) ac bc

instance (Applicative f, ArrowLoop p) => ArrowLoop (Tannen f p) where
  loop = Tannen . fmap loop . runTannen

instance (Applicative f, ArrowZero p) => ArrowZero (Tannen f p) where
  zeroArrow = Tannen $ pure zeroArrow

instance (Applicative f, ArrowPlus p) => ArrowPlus (Tannen f p) where
  Tannen f <+> Tannen g = Tannen (liftA2 (<+>) f g)

-- | @since 5.6.1
instance (Functor f, Swap p) => Swap (Tannen f p) where
  swap = Tannen . fmap swap . runTannen
