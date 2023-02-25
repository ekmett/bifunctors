{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright   :  (C) 2008-2021 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Data.Bifunctor.Tannen
( Tannen(..)
) where

import Control.Applicative
import Control.Arrow as A
import Control.Category
import Control.Comonad
import Data.Bifunctor as B
import Data.Bifunctor.Functor
import Data.Bifunctor.ShowRead
import Data.Bifunctor.Unsafe
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifoldable1 (Bifoldable1(..))
import Data.Bitraversable
import Data.Data
import Data.Foldable1 (Foldable1(..))
import Data.Functor.Classes
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding ((.),id)
import Text.Read (Read (..), readListPrecDefault)

-- | Compose a 'Functor' on the outside of a 'Bifunctor'.
newtype Tannen f p a b = Tannen { runTannen :: f (p a b) }
  deriving stock ( Eq, Ord, Data, Generic, Lift)

deriving stock instance Functor f => Generic1 (Tannen f p a)
deriving stock instance (Functor f, Functor (p a)) => Functor (Tannen f p a)
deriving stock instance (Foldable f, Foldable (p a)) => Foldable (Tannen f p a)
deriving stock instance (Traversable f, Traversable (p a)) => Traversable (Tannen f p a)

instance (Eq1 f, Eq1 (p a)) => Eq1 (Tannen f p a) where
  liftEq eq (Tannen x) (Tannen y) = liftEq (liftEq eq) x y
  {-# inline liftEq #-}

instance (Eq1 f, Eq2 p) => Eq2 (Tannen f p) where
  liftEq2 f g (Tannen x) (Tannen y) = liftEq (liftEq2 f g) x y
  {-# inline liftEq2 #-}

instance (Ord1 f, Ord1 (p a)) => Ord1 (Tannen f p a) where
  liftCompare cmp (Tannen x) (Tannen y) = liftCompare (liftCompare cmp) x y
  {-# inline liftCompare #-}

instance (Ord1 f, Ord2 p) => Ord2 (Tannen f p) where
  liftCompare2 f g (Tannen x) (Tannen y) = liftCompare (liftCompare2 f g) x y
  {-# inline liftCompare2 #-}

instance Show (f (p a b)) => Show (Tannen f p a b) where
  showsPrec = liftShowsPrecWhatever showsPrec

instance Read (f (p a b)) => Read (Tannen f p a b) where
  readPrec = liftReadPrecWhatever readPrec
  readListPrec = readListPrecDefault

instance (Read1 f, Read1 (p a)) => Read1 (Tannen f p a) where
  liftReadPrec rp rl = liftReadPrecWhatever $ liftReadPrec (liftReadPrec rp rl) (liftReadListPrec rp rl)
  liftReadListPrec = liftReadListPrecDefault

instance (Read1 f, Read2 p) => Read2 (Tannen f p) where
  liftReadPrec2 rp1 rl1 rp2 rl2 = liftReadPrecWhatever $
    liftReadPrec (liftReadPrec2 rp1 rl1 rp2 rl2) (liftReadListPrec2 rp1 rl1 rp2 rl2)
  liftReadListPrec2 = liftReadListPrec2Default

instance (Show1 f, Show1 (p a)) => Show1 (Tannen f p a) where
  liftShowsPrec sp sl = liftShowsPrecWhatever $
    liftShowsPrec (liftShowsPrec sp sl) (liftShowList sp sl)

instance (Show1 f, Show2 p) => Show2 (Tannen f p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 = liftShowsPrecWhatever $
    liftShowsPrec (liftShowsPrec2 sp1 sl1 sp2 sl2)
                  (liftShowList2 sp1 sl1 sp2 sl2)

instance Functor f => BifunctorFunctor (Tannen f) where
  bifmap f = Tannen #. fmap f .# runTannen
  {-# inline bifmap #-}

instance (Functor f, Monad f) => BifunctorMonad (Tannen f) where
  bireturn = Tannen #. return
  bibind = \f (Tannen fp) -> Tannen $ fp >>= runTannen . f
  {-# inline bireturn #-}
  {-# inline bibind #-}

instance Comonad f => BifunctorComonad (Tannen f) where
  biextract = extract .# runTannen
  biextend = \f -> Tannen #. extend (f .# Tannen) .# runTannen
  {-# inline biextract #-}
  {-# inline biextend #-}

instance (Functor f, Bifunctor p) => Bifunctor (Tannen f p) where
  first = \f -> Tannen #. fmap (B.first f) .# runTannen
  {-# inline first #-}
  second = \f -> Tannen #. fmap (B.second f) .# runTannen
  {-# inline second #-}
  bimap = \f g -> Tannen #. fmap (bimap f g) .# runTannen
  {-# inline bimap #-}

instance (Applicative f, Biapplicative p) => Biapplicative (Tannen f p) where
  bipure = \a b -> Tannen (pure (bipure a b))
  {-# inline bipure #-}

  (<<*>>) = \fg -> Tannen #. liftA2 (<<*>>) (runTannen fg) .# runTannen
  {-# inline (<<*>>) #-}

  biliftA2 f g = \fg -> Tannen #. liftA2 (biliftA2 f g) (runTannen fg) .# runTannen
  {-# inline biliftA2 #-}

instance (Foldable f, Bifoldable p) => Bifoldable (Tannen f p) where
  bifoldMap f g = foldMap (bifoldMap f g) .# runTannen
  {-# inline bifoldMap #-}

instance (Foldable1 f, Bifoldable1 p) => Bifoldable1 (Tannen f p) where
  bifoldMap1 f g = foldMap1 (bifoldMap1 f g) . runTannen
  {-# INLINE bifoldMap1 #-}

instance (Traversable f, Bitraversable p) => Bitraversable (Tannen f p) where
  bitraverse f g = fmap Tannen . traverse (bitraverse f g) .# runTannen
  {-# inline bitraverse #-}

instance (Applicative f, Category p) => Category (Tannen f p) where
  id = Tannen $ pure id
  (.) = \fg -> Tannen #. liftA2 (.) (runTannen fg) .# runTannen
  {-# inline id #-}
  {-# inline (.) #-}

instance (Applicative f, Arrow p) => Arrow (Tannen f p) where
  arr = Tannen #. pure . arr
  first = Tannen #. fmap A.first .# runTannen
  second = Tannen #. fmap A.second .# runTannen
  (***) = \fg -> Tannen #. liftA2 (***) (runTannen fg) .# runTannen
  (&&&) = \fg -> Tannen #. liftA2 (&&&) (runTannen fg) .# runTannen
  {-# inline arr #-}
  {-# inline first #-}
  {-# inline second #-}
  {-# inline (***) #-}
  {-# inline (&&&) #-}

instance (Applicative f, ArrowChoice p) => ArrowChoice (Tannen f p) where
  left  = Tannen #. fmap left .# runTannen
  right = Tannen #. fmap right .# runTannen
  (+++) = \fg -> Tannen #. liftA2 (+++) (runTannen fg) .# runTannen
  (|||) = \fg -> Tannen #. liftA2 (|||) (runTannen fg) .# runTannen
  {-# inline (|||) #-}
  {-# inline (+++) #-}
  {-# inline left #-}
  {-# inline right #-}

instance (Applicative f, ArrowLoop p) => ArrowLoop (Tannen f p) where
  loop = Tannen #. fmap loop .# runTannen
  {-# inline loop #-}

instance (Applicative f, ArrowZero p) => ArrowZero (Tannen f p) where
  zeroArrow = Tannen $ pure zeroArrow
  {-# inline zeroArrow #-}

instance (Applicative f, ArrowPlus p) => ArrowPlus (Tannen f p) where
  (<+>) = \fg -> Tannen #. liftA2 (<+>) (runTannen fg) .# runTannen
  {-# inline (<+>) #-}

