{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Fix
-- Copyright   :  (C) 2008-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Bifunctor.Fix
  ( Fix(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

-- | Greatest fixpoint of a 'Bifunctor' (a 'Functor' over the first argument with zipping).
newtype Fix p a = In { out :: p (Fix p a) a }
  deriving
    (
#if __GLASGOW_HASKELL__ >= 702
      Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
    , Typeable
#endif
    )

deriving instance Eq   (p (Fix p a) a) => Eq   (Fix p a)
deriving instance Ord  (p (Fix p a) a) => Ord  (Fix p a)
deriving instance Show (p (Fix p a) a) => Show (Fix p a)
deriving instance Read (p (Fix p a) a) => Read (Fix p a)


instance Bifunctor p => Functor (Fix p) where
  fmap f (In p) = In (bimap (fmap f) f p)
  {-# INLINE fmap #-}

instance Biapplicative p => Applicative (Fix p) where
  pure a = In (bipure (pure a) a)
  {-# INLINE pure #-}
  In p <*> In q = In (biliftA2 (<*>) ($) p q)
  {-# INLINE (<*>) #-}

instance Bifoldable p => Foldable (Fix p) where
  foldMap f (In p) = bifoldMap (foldMap f) f p
  {-# INLINE foldMap #-}

instance Bitraversable p => Traversable (Fix p) where
  traverse f (In p) = In <$> bitraverse (traverse f) f p
  {-# INLINE traverse #-}
