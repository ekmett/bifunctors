{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Data.Bifunctor.Flip
( Flip(..)
) where

import qualified Control.Category as Cat
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Functor
import Data.Bitraversable
import Data.Data
import Data.Functor.Classes
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import Text.Read (Read (..))

-- | Make a 'Bifunctor' flipping the arguments of a 'Bifunctor'.
newtype Flip p a b = Flip { runFlip :: p b a }
  deriving ( Eq, Ord
           , Generic, Data, Lift
           )
  deriving (Show, Read) via ShowRead (Flip p a b)

instance (Eq2 p, Eq a) => Eq1 (Flip p a) where
  liftEq = liftEq2 (==)
instance Eq2 p => Eq2 (Flip p) where
  liftEq2 f g (Flip x) (Flip y) = liftEq2 g f x y

instance (Ord2 p, Ord a) => Ord1 (Flip p a) where
  liftCompare = liftCompare2 compare
instance Ord2 p => Ord2 (Flip p) where
  liftCompare2 f g (Flip x) (Flip y) = liftCompare2 g f x y

instance (Read2 p, Read a) => Read1 (Flip p a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec
  liftReadListPrec = liftReadListPrecDefault
instance Read2 p => Read2 (Flip p) where
  liftReadPrec2 rp1 rl1 rp2 rl2 =
    liftReadPrecWhatever (liftReadPrec2 rp2 rl2 rp1 rl1)
  liftReadListPrec2 = liftReadListPrec2Default
instance (Show2 p, Show a) => Show1 (Flip p a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 p => Show2 (Flip p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 =
    liftShowsPrecWhatever $ liftShowsPrec2 sp2 sl2 sp1 sl1

instance Bifunctor p => Bifunctor (Flip p) where
  first f = Flip . second f . runFlip
  {-# INLINE first #-}
  second f = Flip . first f . runFlip
  {-# INLINE second #-}
  bimap f g = Flip . bimap g f . runFlip
  {-# INLINE bimap #-}

instance Bifunctor p => Functor (Flip p a) where
  fmap f = Flip . first f . runFlip
  {-# INLINE fmap #-}

instance Biapplicative p => Biapplicative (Flip p) where
  bipure a b = Flip (bipure b a)
  {-# INLINE bipure #-}

  Flip fg <<*>> Flip xy = Flip (fg <<*>> xy)
  {-# INLINE (<<*>>) #-}

  biliftA2 f g (Flip xy) (Flip ab) = Flip $ biliftA2 g f xy ab
  {-# INLINE biliftA2 #-}

instance Bifoldable p => Bifoldable (Flip p) where
  bifoldMap f g = bifoldMap g f . runFlip
  {-# INLINE bifoldMap #-}

instance Bifoldable p => Foldable (Flip p a) where
  foldMap f = bifoldMap f (const mempty) . runFlip
  {-# INLINE foldMap #-}

instance Bitraversable p => Bitraversable (Flip p) where
  bitraverse f g = fmap Flip . bitraverse g f . runFlip
  {-# INLINE bitraverse #-}

instance Bitraversable p => Traversable (Flip p a) where
  traverse f = fmap Flip . bitraverse f pure . runFlip
  {-# INLINE traverse #-}

instance BifunctorFunctor Flip where
  bifmap f (Flip p) = Flip (f p)
  {-# INLINE bifmap #-}

instance Cat.Category c => Cat.Category (Flip c) where
  id = Flip Cat.id
  {-# INLINE id #-}
  Flip x . Flip y = Flip (y Cat.. x)
  {-# INLINE (.) #-}

