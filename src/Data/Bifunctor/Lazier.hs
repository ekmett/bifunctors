{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PolyKinds #-}

-- | A newtype wrapper to make lazier bifunctors.
--
-- @since 6
module Data.Bifunctor.Lazier
  ( Lazier (..)
  , Lazify (..)) where

import GHC.Generics
import Control.Applicative (Applicative (..), Const (..))
import Data.Bifunctor
import Data.Biapplicative
import GHC.TypeLits
import Data.Coerce
import qualified Data.Functor.Product as Product
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import Data.Type.Equality ((:~:)(..), (:~~:)(..), type (~~))
import Type.Reflection (Typeable, TypeRep, typeRep)
#if MIN_VERSION_base (4,15,0)
import GHC.Tuple (Solo (..))
#endif
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Biff (Biff)
import Data.Bifunctor.Wrapped (WrappedBifunctor)
import Data.Bifunctor.Flip (Flip)
import qualified Data.Bifunctor.Fix as BiFix
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Join (Join)
import Data.Bifunctor.Clown (Clown)
import Data.Bifunctor.Biap (Biap)
import qualified Data.Bifunctor.Product as BiProduct
import qualified Data.Semigroup as Semigroup
import qualified Data.Monoid as Monoid
import Data.Tagged (Tagged)
import Data.Tree (Tree)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Functor.Reverse as FuncReverse
import qualified Control.Applicative.Backwards as AppBackwards
import Control.Monad.Trans.Identity (IdentityT)

-- | Given a bifunctor @p@ with a 'Lazify' instance (typically a tuple-like
-- type with lazy fields), produce a bifunctor whose 'Bifunctor',
-- 'Biapplicative', 'Functor', and 'Applicative' methods are lazy in their
-- @Lazier p a b@ argument(s).
--
-- For example, given
--
-- @
-- data SP a b = SP a b
--   deriving (Functor, Generic)
--
-- instance Bifunctor SP where
--   bimap f g (SP a b) = SP (f a) (g b)
-- @
--
-- we effectively get
--
-- @
-- bimap f g ~(Lazier (SP a b)) = Lazier (SP (f a) (g b))
-- fmap f ~(Lazier (SP a b)) = Lazier (SP a (f b))
-- @
--
-- === Notes
--
-- If the type is a newtype, then it is required to wrap a
-- lazy generic product type, etc.
--
-- We don't lazify the results of @pure@ or @bipure@. In a lazifiable
-- bifunctor, these functions should always be lazy anyway.
--
-- === The details
--
-- The @Lazier@ arguments are "lazified" before being passed to the
-- underlying 'Bifunctor', etc., method using 'lazify'. So
--
-- @
-- 'bimap' f g (Lazier m) = Lazier ('bimap' f g ('lazify' m))
-- 'biliftA2' f g (Lazier m) (Lazier n) = Lazier ('biliftA2' f g ('lazify' m) ('lazify' n))
-- @
--
-- The default 'lazify' implementation is sufficient for simple tuple types. It
-- does not, however, recursively lazify nested tuples, or work any sort of
-- magic with strict fields that happen to be singletons.  To make those 
-- sufficiently lazy, users must write custom instances of 'Lazify'.
--
-- @since 6
newtype Lazier p a b = Lazier { unLazier :: p a b }
  deriving (Show, Read, Eq, Ord)

instance (Functor (p a), forall b. Lazify (p a b)) => Functor (Lazier p a) where
  fmap f (Lazier x) = Lazier (fmap f (lazify x))
  x <$ Lazier m = Lazier (x <$ lazify m)

instance (Bifunctor p, forall a b. Lazify (p a b)) => Bifunctor (Lazier p) where
  bimap f g (Lazier x) = Lazier (bimap f g (lazify x))
  first f (Lazier x) = Lazier (first f (lazify x))
  second g (Lazier x) = Lazier (second g (lazify x))

instance (Applicative (p a), forall b. Lazify (p a b)) => Applicative (Lazier p a) where
  pure :: forall b. b -> Lazier p a b
  pure = coerce (pure @(p a) @b)

  Lazier fs <*> Lazier xs = Lazier (lazify fs <*> lazify xs)
  liftA2 f (Lazier xs) (Lazier ys) = Lazier (liftA2 f (lazify xs) (lazify ys))

  Lazier xs *> Lazier ys = Lazier (lazify xs *> lazify ys)
  Lazier xs <* Lazier ys = Lazier (lazify xs <* lazify ys)

instance (Biapplicative p, forall a b. Lazify (p a b)) => Biapplicative (Lazier p) where
  bipure :: forall a b. a -> b -> Lazier p a b
  bipure = coerce (bipure @p @a @b)

  Lazier fs <<*>> Lazier xs = Lazier (lazify fs <<*>> lazify xs)
  biliftA2 f g (Lazier xs) (Lazier ys) = Lazier (biliftA2 f g (lazify xs) (lazify ys))

  Lazier xs *>> Lazier ys = Lazier (lazify xs *>> lazify ys)
  Lazier xs <<* Lazier ys = Lazier (lazify xs <<* lazify ys)

-- | Tuple-like types and newtypes around such that can be made lazy.  There is
-- a default implementation which works for most lazifiable types that are
-- instances of `Generic`. In the event that you need an instance for a type
-- for which that doesn't work, you can write a custom one. For example,

-- @
-- data Baz = Beep !Void | Boop Int
-- @
--
-- can't be lazified generically, but you can write
--
-- @
-- instance Lazify Baz where
--   lazify ~(Boop i) = Boop i
-- @
--
-- Similarly, if a tuple has one or more strict fields, but
-- they're all /singletons/, then a custom 'Lazify' instance can
-- pull their values out of the air given the right constraints.
--
-- @since 6
class Lazify a where
  -- Make the value of a product type lazy. For example, for pairs,
  -- it is (effectively) defined
  --
  -- @
  -- lazify ~(a, b) = (a, b)
  -- @
  lazify :: a -> a
  default lazify :: (Generic a, GLazify a (Rep a)) => a -> a
  lazify = to . glazify @a . from

instance a ~ b => Lazify (a :~: b) where
  lazify _ = Refl
instance a ~~ b => Lazify (a :~~: b) where
  lazify _ = HRefl
instance Typeable a => Lazify (TypeRep a) where
  lazify _ = typeRep

-- Boring, generic-derived instances are at the bottom.

class GLazify a f where
  glazify :: f p -> f p

instance GLazify a f => GLazify a (D1 ('MetaData _q _r _s 'False) f) where
  glazify (M1 m) = M1 (glazify @a m)
instance GLazify a f => GLazify a (C1 c f) where
  glazify (M1 m) = M1 (glazify @a m)
instance GLazify a f => GLazify a (S1 ('MetaSel _p _q _r 'DecidedLazy) f) where
  glazify (M1 m) = M1 (glazify @a m)
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It has a strict field.")
  => GLazify a (S1 ('MetaSel _p _q _r 'DecidedStrict) f) where
  glazify _ = error "Unreachable"
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It has a strict (unpacked) field.")
  => GLazify a (S1 ('MetaSel _p _q _r 'DecidedUnpack) f) where
  glazify _ = error "Unreachable"

-- Newtypes delegate to what they wrap. In the process, we lose
-- track of the wrapping type, so if someone writes
--
-- newtype Foo a b = Foo (Bar a b) deriving Generic
-- data Bar a b = L a | R b deriving Generic
--
-- then lazify (Foo ...) will produce a type error about Bar rather than
-- about Foo. This is arguably not ideal (it would be nice to get a whole
-- explanation of what wraps what and so on), but it's necessary if users
-- are to be able to write custom instances for Lazify without some
-- additional major infrastructure and likely user pain.
instance Lazify c
  => GLazify a (D1 ('MetaData _q _r _s 'True) (C1 _t (S1 _u (K1 _v c)))) where
  glazify (M1 (M1 (M1 (K1 x)))) = M1 (M1 (M1 (K1 (lazify x))))

instance (GLazify a f, GLazify a g) => GLazify a (f :*: g) where
  glazify ~(m :*: f) = m :*: f

instance GLazify a U1 where
  glazify ~U1 = U1

instance GLazify a (K1 i c) where
  -- These just pass through.
  glazify x = x

instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It is a sum type.")
  => GLazify a (f :+: g) where
  glazify _ = error "Unreachable"

-- Derived instances

instance Lazify ()
#if MIN_VERSION_base (4,15,0)
instance Lazify (Solo a)
#endif
instance Lazify (a, b)
instance Lazify (a, b, c)
instance Lazify (a, b, c, d)
instance Lazify (a, b, c, d, e)
instance Lazify (a, b, c, d, e, f)
instance Lazify (a, b, c, d, e, f, g)
instance Lazify (Product.Product f g a)
instance Lazify (BiProduct.Product f g a b)
instance Lazify (Semigroup.Arg a b)
instance Lazify (NonEmpty a)
instance Lazify (Tree a)
instance Lazify a => Lazify (Identity a)
instance Lazify a => Lazify (Const a b)
instance Lazify b => Lazify (Tagged a b)
instance Lazify (f (g a)) => Lazify (Compose f g a)
instance Lazify ((f :*: g) a)
instance Lazify (f (g a)) => Lazify ((f :.: g) a)
instance Lazify (p a b) => Lazify (WrappedBifunctor p a b)
instance Lazify (f (p a b)) => Lazify (Tannen f p a b)
instance Lazify (f a) => Lazify (Clown f a b)
instance Lazify (g b) => Lazify (Joker g a b)
instance Lazify (p a a) => Lazify (Join p a)
instance Lazify (p b a) => Lazify (Flip p a b)
instance Lazify (p (BiFix.Fix p a) a) => Lazify (BiFix.Fix p a)
instance Lazify (p (f a) (g b)) => Lazify (Biff p f g a b)
instance Lazify (bi a b) => Lazify (Biap bi a b)
instance Lazify a => Lazify (Semigroup.Product a)
instance Lazify a => Lazify (Semigroup.Sum a)
instance Lazify a => Lazify (Semigroup.Dual a)
instance Lazify a => Lazify (Semigroup.WrappedMonoid a)
instance Lazify a => Lazify (Semigroup.Last a)
instance Lazify a => Lazify (Semigroup.First a)
instance Lazify a => Lazify (Semigroup.Min a)
instance Lazify a => Lazify (Semigroup.Max a)
instance Lazify (f a) => Lazify (Monoid.Alt f a)
instance Lazify (f a) => Lazify (Monoid.Ap f a)
instance Lazify (t a) => Lazify (FuncReverse.Reverse t a)
instance Lazify (f a) => Lazify (AppBackwards.Backwards f a)
instance Lazify (f a) => Lazify (IdentityT f a)
