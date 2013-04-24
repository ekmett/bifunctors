-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Product
-- Copyright   :  (C) 2008-2013 Jesse Selover,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The product of two bifunctors.
----------------------------------------------------------------------------
module Data.Bifunctor.Product
  ( Product(..)
  ) where

import Control.Applicative
import Data.Biapplicative
import Data.Functor.Apply
import Data.Bifoldable
import Data.Bitraversable
import Data.Monoid hiding (Product, (<>))
import Data.Semigroup hiding (Product)
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable

-- | Form the product of two bifunctors
data Product f g a b = Pair { left :: f a b
                            , right :: g a b
                            } deriving (Eq,Ord,Show,Read)

instance (Bifunctor f, Bifunctor g) => Bifunctor (Product f g) where
  first f (Pair x y) = Pair (first f x) (first f y)
  {-# INLINE first #-}
  second g (Pair x y) = Pair (second g x) (second g y)
  {-# INLINE second #-}
  bimap f g (Pair x y) = Pair (bimap f g x) (bimap f g y)
  {-# INLINE bimap #-}

instance (Biapplicative f, Biapplicative g) => Biapplicative (Product f g) where
  bipure a b = Pair (bipure a b) (bipure a b)
  {-# INLINE bipure #-}
  Pair w x <<*>> Pair y z = Pair (w <<*>> y) (x <<*>> z)
  {-# INLINE (<<*>>) #-}

instance (Bifoldable f, Bifoldable g) => Bifoldable (Product f g) where
  bifoldMap f g (Pair x y) = bifoldMap f g x `mappend` bifoldMap f g y
  {-# INLINE bifoldMap #-}

instance (Bitraversable f, Bitraversable g) => Bitraversable (Product f g) where
  bitraverse f g (Pair x y) = Pair <$> bitraverse f g x <*> bitraverse f g y
  {-# INLINE bitraverse #-}

instance (Bifoldable1 f, Bifoldable1 g) => Bifoldable1 (Product f g) where
  bifoldMap1 f g (Pair x y) = bifoldMap1 f g x <> bifoldMap1 f g y
  {-# INLINE bifoldMap1 #-}

instance (Bitraversable1 f, Bitraversable1 g) => Bitraversable1 (Product f g) where
  bitraverse1 f g (Pair x y) = Pair <$> bitraverse1 f g x <.> bitraverse1 f g y
  {-# INLINE bitraverse1 #-}
