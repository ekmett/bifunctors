{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Trustworthy #-}

-- | A newtype wrapper to make stricter bifunctors.
--
-- @since 6
module Data.Bifunctor.Stricter
  ( Stricter (..)
  ) where
import Data.Bifunctor
import Data.Biapplicative
import Control.Applicative (Applicative (..))
import Data.Coerce

-- | Given a bifunctor @f@, produce a bifunctor whose 'Bifunctor',
-- 'Biapplicative', 'Functor', and 'Applicative' methods are strict
-- in their @f a b@ argument(s).
--
-- For example, we effectively have
--
-- @
-- instance 'Bifunctor' (Stricter (,)) where
--   'bimap' f g (a, b) = (f a, g b)
-- @
--
-- ### Note
--
-- This may give unexpected results for "bare" newtypes, whose natural
-- instances are in fact lazy. For example,
--
-- @Stricter (Const (const 3)) <<*>> undefined = undefined@
--
-- @since 6
newtype Stricter f a b = Stricter { unStricter :: f a b }
  deriving (Eq, Ord, Show)

instance Functor (f a) => Functor (Stricter f a) where
  fmap f (Stricter !m) = Stricter (fmap f m)
  x <$ (Stricter !m) = Stricter (x <$ m)

instance Applicative (f a) => Applicative (Stricter f a) where
  pure :: forall b. b -> Stricter f a b
  pure = coerce (pure :: b -> f a b)

  Stricter (!fs) <*> Stricter (!xs) = Stricter (fs <*> xs)
  liftA2 f (Stricter (!xs)) (Stricter (!ys)) = Stricter (liftA2 f xs ys)

  Stricter (!xs) *> Stricter (!ys) = Stricter (xs *> ys)
  Stricter (!xs) <* Stricter (!ys) = Stricter (xs <* ys)

instance Bifunctor f => Bifunctor (Stricter f) where
  bimap f g !(Stricter m) = Stricter (bimap f g m)
  first f !(Stricter m) = Stricter (first f m)
  second g !(Stricter m) = Stricter (second g m)

instance Biapplicative f => Biapplicative (Stricter f) where
  bipure :: forall a b. a -> b -> Stricter f a b
  bipure = coerce (bipure :: a -> b -> f a b)

  Stricter (!fs) <<*>> Stricter (!as) = Stricter (fs <<*>> as)
  biliftA2 f g !(Stricter xs) !(Stricter ys) = Stricter (biliftA2 f g xs ys)

  Stricter (!xs) *>> Stricter (!ys) = Stricter (xs *>> ys)
  Stricter (!xs) <<* Stricter (!ys) = Stricter (xs <<* ys)
