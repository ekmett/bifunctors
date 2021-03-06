{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Data.Bifunctor.Biff
( Biff(..)
) where

import Control.Applicative
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Unsafe
import Data.Bitraversable
import Data.Data
import Data.Functor.Classes
import GHC.Generics

-- | Compose two 'Functor's on the inside of a 'Bifunctor'.
newtype Biff p f g a b = Biff { runBiff :: p (f a) (g b) }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

deriving stock instance Functor (p (f a)) => Generic1 (Biff p f g a)
deriving stock instance (Functor (p (f a)), Functor g) => Functor (Biff p f g a)
deriving stock instance (Foldable (p (f a)), Foldable g) => Foldable (Biff p f g a)
deriving stock instance (Traversable (p (f a)), Traversable g) => Traversable (Biff p f g a)

instance (Eq2 p, Eq1 f, Eq1 g, Eq a) => Eq1 (Biff p f g a) where
  liftEq = liftEq2 (==)

instance (Eq2 p, Eq1 f, Eq1 g) => Eq2 (Biff p f g) where
  liftEq2 f g (Biff x) (Biff y) = liftEq2 (liftEq f) (liftEq g) x y

instance (Ord2 p, Ord1 f, Ord1 g, Ord a) => Ord1 (Biff p f g a) where
  liftCompare = liftCompare2 compare

instance (Ord2 p, Ord1 f, Ord1 g) => Ord2 (Biff p f g) where
  liftCompare2 f g (Biff x) (Biff y) = liftCompare2 (liftCompare f) (liftCompare g) x y

instance (Read2 p, Read1 f, Read1 g, Read a) => Read1 (Biff p f g a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList

instance (Read2 p, Read1 f, Read1 g) => Read2 (Biff p f g) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 p = readParen (p > 10) $ \s0 -> do
    ("Biff",    s1) <- lex s0
    ("{",       s2) <- lex s1
    ("runBiff", s3) <- lex s2
    (x,         s4) <- liftReadsPrec2 (liftReadsPrec rp1 rl1) (liftReadList rp1 rl1)
                                      (liftReadsPrec rp2 rl2) (liftReadList rp2 rl2) 0 s3
    ("}",       s5) <- lex s4
    return (Biff x, s5)

instance (Show2 p, Show1 f, Show1 g, Show a) => Show1 (Biff p f g a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show2 p, Show1 f, Show1 g) => Show2 (Biff p f g) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (Biff x) = showParen (p > 10) $
      showString "Biff {runBiff = "
    . liftShowsPrec2 (liftShowsPrec sp1 sl1) (liftShowList sp1 sl1)
                     (liftShowsPrec sp2 sl2) (liftShowList sp2 sl2) 0 x
    . showChar '}'

instance (Bifunctor p, Functor f, Functor g) => Bifunctor (Biff p f g) where
  first = \f -> Biff #. first (fmap f) .# runBiff
  {-# inline first #-}
  second = \f -> Biff #. second (fmap f) .# runBiff
  {-# inline second #-}
  bimap = \f g -> Biff #. bimap (fmap f) (fmap g) .# runBiff
  {-# inline bimap #-}

instance (Biapplicative p, Applicative f, Applicative g) => Biapplicative (Biff p f g) where
  bipure a b = Biff (bipure (pure a) (pure b))
  {-# inline bipure #-}
  biliftA2 = \f g (Biff x) -> Biff #. biliftA2 (liftA2 f) (liftA2 g) x .# runBiff
  {-# inline biliftA2 #-}
  (<<*>>) = \(Biff fg) -> Biff #. biliftA2 (<*>) (<*>) fg .# runBiff
  {-# inline (<<*>>) #-}

instance (Bifoldable p, Foldable f, Foldable g) => Bifoldable (Biff p f g) where
  bifoldMap f g = bifoldMap (foldMap f) (foldMap g) .# runBiff
  {-# inline bifoldMap #-}

instance (Bitraversable p, Traversable f, Traversable g) => Bitraversable (Biff p f g) where
  bitraverse f g = fmap Biff . bitraverse (traverse f) (traverse g) .# runBiff
  {-# inline bitraverse #-}
