{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

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
  , traverse2
  , traverse2With
  , module Data.Bifunctor
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Functor.Identity

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
import Data.Traversable (Traversable (traverse))
#endif

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_semigroups(0,16,2)
import Data.Semigroup (Arg(..))
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
  -- a '*>>' b ≡ 'bimap' ('const' 'id') ('const' 'id') '<<$>>' a '<<*>>' b
  -- @
  (*>>) :: p a b -> p c d -> p c d
  a *>> b = bimap (const id) (const id) <<$>> a <<*>> b
  {-# INLINE (*>>) #-}

  -- |
  -- @
  -- a '<<*' b ≡ 'bimap' 'const' 'const' '<<$>>' a '<<*>>' b
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

-- | Traverse a 'Traversable' container in a 'Biapplicative'.
traverse2 :: forall t a b c p. (Traversable t, Biapplicative p)
          => (a -> p b c) -> t a -> p (t b) (t c)
traverse2 = traverse2With traverse
-- We use a staged INLINE so we can rewrite traverse2 to specialized versions
-- for important types.
{-# INLINABLE [1] traverse2 #-}

-- | A version of 'traverse2' that doesn't care how the traversal is
-- done.
--
-- @
-- traverse2 = traverse2With traverse
-- @
traverse2With :: forall s t a b c p. Biapplicative p
  => (forall f x. Applicative f => (a -> f x) -> s -> f (t x))
  -> (a -> p b c) -> s -> p (t b) (t c)
traverse2With trav p s = go m m
  where
    m :: Mag a x (t x)
    m = trav One s

    go :: forall x y. Mag a b x -> Mag a c y -> p x y
    go (Pure t) (Pure u) = bipure t u
    go (Map f x) (Map g y) = bimap f g (go x y)
    go (Ap fs xs) (Ap gs ys) = go fs gs <<*>> go xs ys
#if MIN_VERSION_base(4,10,0)
    go (LiftA2 f xs ys) (LiftA2 g zs ws) = bimap f g (go xs zs) <<*>> go ys ws
#endif
    go (One x) (One _) = p x
    go _ _ = error "Impossible: the arguments are always the same."
{-# INLINABLE traverse2With #-}

-- This is used to reify a traversal for 'traverse2'. It's a somewhat
-- bogus 'Functor' and 'Applicative' closely related to 'Magma' from the
-- @lens@ package. Valid traversals don't use (<$), (<*), or (*>), so
-- we leave them out. We offer all the rest of the Functor and Applicative
-- operations to improve performance: we generally want to keep the structure
-- as small as possible. We might even consider using RULES to widen lifts
-- when we can:
--
--   f <$> x <*> y      ==> liftA2 f x y,
--   liftA2 f x y <*> z ==> liftA3 f x y z,
--
-- etc., up to the pointer tagging limit. But we do need to be careful. I don't
-- *think* GHC will ever inline the traversal into the go function (because that
-- would duplicate work), but if it did, and if different RULES fired for the
-- two copies, everything would break horribly.
--
-- Note: if it's necessary for some reason, we *could* relax GADTs to
-- ExistentialQuantification by changing the type of One to
--
--   One :: (b -> c) -> a -> Mag a b c
--
-- where the function will always end up being id. But we allocate a *lot*
-- of One constnructors, so this would definitely be bad for performance.
data Mag a b t where
  Pure :: t -> Mag a b t
  Map :: (x -> t) -> Mag a b x -> Mag a b t
  Ap :: Mag a b (t -> u) -> Mag a b t -> Mag a b u
#if MIN_VERSION_base(4,10,0)
  LiftA2 :: (t -> u -> v) -> Mag a b t -> Mag a b u -> Mag a b v
#endif
  One :: a -> Mag a b b

instance Functor (Mag a b) where
  fmap = Map

instance Applicative (Mag a b) where
  pure = Pure
  (<*>) = Ap
#if MIN_VERSION_base(4,10,0)
  liftA2 = LiftA2
#endif

-- Rewrite rules for a few important types.

traverse2List :: Biapplicative p => (a -> p b c) -> [a] -> p [b] [c]
traverse2List f = foldr go (bipure [] [])
  where
    go x r = biliftA2 (:) (:) (f x) r
{-# RULES
"traverse2/list" forall f t. traverse2 f t = traverse2List f t
 #-}

traverse2Maybe :: Biapplicative p => (a -> p b c) -> Maybe a -> p (Maybe b) (Maybe c)
traverse2Maybe _f Nothing = bipure Nothing Nothing
traverse2Maybe f (Just x) = bimap Just Just (f x)
{-# RULES
"traverse2/Maybe" forall f t. traverse2 f t = traverse2Maybe f t
 #-}

traverse2Either :: Biapplicative p => (a -> p b c) -> Either e a -> p (Either e b) (Either e c)
traverse2Either _f (Left e) = bipure (Left e) (Left e)
traverse2Either f (Right x) = bimap Right Right (f x)
{-# RULES
"traverse2/Either" forall f t. traverse2 f t = traverse2Either f t
 #-}

traverse2Identity :: Biapplicative p => (a -> p b c) -> Identity a -> p (Identity b) (Identity c)
traverse2Identity f (Identity x) = bimap Identity Identity (f x)
{-# RULES
"traverse2/Identity" forall f t. traverse2 f t = traverse2Identity f t
 #-}

traverse2Const :: Biapplicative p => (a -> p b c) -> Const x a -> p (Const x b) (Const x c)
traverse2Const _f (Const x) = bipure (Const x) (Const x)
{-# RULES
"traverse2/Const" forall f t. traverse2 f t = traverse2Const f t
 #-}

traverse2Pair :: Biapplicative p => (a -> p b c) -> (e, a) -> p (e, b) (e, c)
traverse2Pair f (x,y) = bimap ((,) x) ((,) x) (f y)
{-# RULES
"traverse2/Pair" forall f t. traverse2 f t = traverse2Pair f t
 #-}

----------------------------------------------
--
-- Instances

instance Biapplicative (,) where
  bipure = (,)
  {-# INLINE bipure #-}
  (f, g) <<*>> (a, b) = (f a, g b)
  {-# INLINE (<<*>>) #-}

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_semigroups(0,16,2)
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
