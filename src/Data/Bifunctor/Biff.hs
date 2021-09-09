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
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Unsafe
import Data.Bitraversable
import Data.Data
import Data.Functor.Classes
import GHC.Generics
import Text.Read (Read (..), readListPrecDefault)

-- | Compose two 'Functor's on the inside of a 'Bifunctor'.
newtype Biff p f g a b = Biff { runBiff :: p (f a) (g b) }
  deriving stock (Eq, Ord, Data, Generic)

deriving stock instance Functor (p (f a)) => Generic1 (Biff p f g a)
deriving stock instance (Functor (p (f a)), Functor g) => Functor (Biff p f g a)
deriving stock instance (Foldable (p (f a)), Foldable g) => Foldable (Biff p f g a)
deriving stock instance (Traversable (p (f a)), Traversable g) => Traversable (Biff p f g a)

instance (Eq1 (p (f a)), Eq1 g) => Eq1 (Biff p f g a) where
  liftEq eq (Biff x) (Biff y) = liftEq (liftEq eq) x y

instance (Eq2 p, Eq1 f, Eq1 g) => Eq2 (Biff p f g) where
  liftEq2 f g (Biff x) (Biff y) = liftEq2 (liftEq f) (liftEq g) x y

instance (Ord1 (p (f a)), Ord1 g) => Ord1 (Biff p f g a) where
  liftCompare cmp (Biff x) (Biff y) = liftCompare (liftCompare cmp) x y

instance (Ord2 p, Ord1 f, Ord1 g) => Ord2 (Biff p f g) where
  liftCompare2 f g (Biff x) (Biff y) = liftCompare2 (liftCompare f) (liftCompare g) x y

instance Show (p (f a) (g b)) => Show (Biff p f g a b) where
  showsPrec = liftShowsPrecWhatever showsPrec

instance Read (p (f a) (g b)) => Read (Biff p f g a b) where
  readPrec = liftReadPrecWhatever readPrec
  readListPrec = readListPrecDefault

instance (Show1 (p (f a)), Show1 g) => Show1 (Biff p f g a) where
  liftShowsPrec sp sl = liftShowsPrecWhatever $ liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)

instance (Read1 (p (f a)), Read1 g) => Read1 (Biff p f g a) where
  liftReadPrec rp rl = liftReadPrecWhatever (liftReadPrec (liftReadPrec rp rl) (liftReadListPrec rp rl))
  liftReadListPrec = liftReadListPrecDefault

instance (Show2 p, Show1 f, Show1 g) => Show2 (Biff p f g) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 = liftShowsPrecWhatever $
    liftShowsPrec2
      (liftShowsPrec sp1 sl1) (liftShowList sp1 sl1)
      (liftShowsPrec sp2 sl2) (liftShowList sp2 sl2)

instance (Read2 p, Read1 f, Read1 g) => Read2 (Biff p f g) where
  liftReadPrec2 rp1 rl1 rp2 rl2 =
    liftReadPrecWhatever
      (liftReadPrec2
         (liftReadPrec rp1 rl1) (liftReadListPrec rp1 rl1)
         (liftReadPrec rp2 rl2) (liftReadListPrec rp2 rl2))
  liftReadListPrec2 = liftReadListPrec2Default

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
