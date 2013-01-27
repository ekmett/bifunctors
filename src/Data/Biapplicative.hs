-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Apply
-- Copyright   :  (C) 2011-2013 Edward Kmett,
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

import Data.Bifunctor
import Data.Bifunctor.Apply ((<<$>>))
import Data.Tagged

infixl 4 <<*>>, <<*, *>>, <<**>>

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
  (f, g) <<*>> (a, b) = (f a, g b)
  {-# INLINE (<<*>>) #-}

instance Biapplicative Tagged where
  bipure _ b = Tagged b
  {-# INLINE bipure #-}

  Tagged f <<*>> Tagged x = Tagged (f x)
  {-# INLINE (<<*>>) #-}
