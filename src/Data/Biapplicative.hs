{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_semigroups
#define MIN_VERSION_semigroups(x,y,z) 0
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Biapplicative (
  -- * Biapplicative bifunctors
    Biapplicative(..)
  , (<<$>>)
  , (<<**>>)
  , biliftA2
  , biliftA3
  , module Data.Bifunctor
  ) where

import Control.Applicative
import Data.Bifunctor

#if MIN_VERSION_semigroups(0,16,2)
import Data.Semigroup
#else
import Data.Monoid
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

infixl 4 <<$>>, <<*>>, <<*, *>>, <<**>>
(<<$>>) :: (a -> b) -> a -> b
(<<$>>) = id
{-# INLINE (<<$>>) #-}

class Bifunctor p => Biapplicative p where
  bipure :: a -> b -> p a b

  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d

  -- |
  -- @
  -- a '*>' b ≡ 'const' 'id' '<$>' a '<*>' b
  -- @
  (*>>) :: p a b -> p c d -> p c d
  a *>> b = bimap (const id) (const id) <<$>> a <<*>> b
  {-# INLINE (*>>) #-}

  -- |
  -- @
  -- a '<*' b ≡ 'const' '<$>' a '<.>' b
  -- @
  (<<*) :: p a b -> p c d -> p a b
  a <<* b = bimap const const <<$>> a <<*>> b
  {-# INLINE (<<*) #-}

(<<**>>) :: Biapplicative p => p a c -> p (a -> b) (c -> d) -> p b d
(<<**>>) = biliftA2 (flip id) (flip id)
{-# INLINE (<<**>>) #-}

-- | Lift binary functions
biliftA2 :: Biapplicative w => (a -> b -> c) -> (d -> e -> f) -> w a d -> w b e -> w c f
biliftA2 f g a b = bimap f g <<$>> a <<*>> b
{-# INLINE biliftA2 #-}

-- | Lift ternary functions
biliftA3 :: Biapplicative w => (a -> b -> c -> d) -> (e -> f -> g -> h) -> w a e -> w b f -> w c g -> w d h
biliftA3 f g a b c = bimap f g <<$>> a <<*>> b <<*>> c
{-# INLINE biliftA3 #-}

instance Biapplicative (,) where
  bipure = (,)
  {-# INLINE bipure #-}
  (f, g) <<*>> (a, b) = (f a, g b)
  {-# INLINE (<<*>>) #-}

#if MIN_VERSION_semigroups(0,16,2)
instance Biapplicative Arg where
  bipure = Arg
  {-# INLINE bipure #-}
  Arg f g <<*>> Arg a b = Arg (f a) (g b)
  {-# INLINE (<<*>>) #-}
#endif

instance Monoid x => Biapplicative ((,,) x) where
  bipure = (,,) mempty
  {-# INLINE bipure #-}
  (x, f, g) <<*>> (x', a, b) = (mappend x x', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y) => Biapplicative ((,,,) x y) where
  bipure = (,,,) mempty mempty
  {-# INLINE bipure #-}
  (x, y, f, g) <<*>> (x', y', a, b) = (mappend x x', mappend y y', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y, Monoid z) => Biapplicative ((,,,,) x y z) where
  bipure = (,,,,) mempty mempty mempty
  {-# INLINE bipure #-}
  (x, y, z, f, g) <<*>> (x', y', z', a, b) = (mappend x x', mappend y y', mappend z z', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y, Monoid z, Monoid w) => Biapplicative ((,,,,,) x y z w) where
  bipure = (,,,,,) mempty mempty mempty mempty
  {-# INLINE bipure #-}
  (x, y, z, w, f, g) <<*>> (x', y', z', w', a, b) = (mappend x x', mappend y y', mappend z z', mappend w w', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y, Monoid z, Monoid w, Monoid v) => Biapplicative ((,,,,,,) x y z w v) where
  bipure = (,,,,,,) mempty mempty mempty mempty mempty
  {-# INLINE bipure #-}
  (x, y, z, w, v, f, g) <<*>> (x', y', z', w', v', a, b) = (mappend x x', mappend y y', mappend z z', mappend w w', mappend v v', f a, g b)
  {-# INLINE (<<*>>) #-}

#ifdef MIN_VERSION_tagged
instance Biapplicative Tagged where
  bipure _ b = Tagged b
  {-# INLINE bipure #-}

  Tagged f <<*>> Tagged x = Tagged (f x)
  {-# INLINE (<<*>>) #-}
#endif

instance Biapplicative Const where
  bipure a _ = Const a
  {-# INLINE bipure #-}
  Const f <<*>> Const x = Const (f x)
  {-# INLINE (<<*>>) #-}
