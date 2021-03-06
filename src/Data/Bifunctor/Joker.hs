{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- From the Functional Pearl \"Clowns to the Left of me, Jokers to the Right: Dissecting Data Structures\"
-- by Conor McBride.

module Data.Bifunctor.Joker
( Joker(..)
) where

import Data.Bifunctor
import Data.Bifunctor.Unsafe
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Coerce
import Data.Data
import Data.Functor.Classes
import GHC.Generics

-- | Make a 'Functor' over the second argument of a 'Bifunctor'.
--
-- Mnemonic: C__l__owns to the __l__eft (parameter of the Bifunctor),
--           joke__r__s to the __r__ight.
newtype Joker g a b = Joker { runJoker :: g b }
  deriving ( Eq, Ord, Show, Read, Data
           , Generic
           , Generic1
           )

instance Eq1 g => Eq1 (Joker g a) where
  liftEq = eqJoker #. liftEq
  {-# inline liftEq #-}

instance Eq1 g => Eq2 (Joker g) where
  liftEq2 = \_ -> eqJoker #. liftEq
  {-# inline liftEq2 #-}

instance Ord1 g => Ord1 (Joker g a) where
  liftCompare = compareJoker #. liftCompare
  {-# inline liftCompare #-}

instance Ord1 g => Ord2 (Joker g) where
  liftCompare2 _ = compareJoker #. liftCompare
  {-# inline liftCompare2 #-}

instance Read1 g => Read1 (Joker g a) where
  liftReadsPrec rp rl = readsPrecJoker (liftReadsPrec rp rl)

instance Read1 g => Read2 (Joker g) where
  liftReadsPrec2 _ _ rp2 rl2 = readsPrecJoker (liftReadsPrec rp2 rl2)

instance Show1 g => Show1 (Joker g a) where
  liftShowsPrec sp sl = showsPrecJoker (liftShowsPrec sp sl)

instance Show1 g => Show2 (Joker g) where
  liftShowsPrec2 _ _ sp2 sl2 = showsPrecJoker (liftShowsPrec sp2 sl2)

eqJoker :: (g b1 -> g b2 -> Bool)
        -> Joker g a1 b1 -> Joker g a2 b2 -> Bool
eqJoker = coerce
{-# inline eqJoker #-}

compareJoker :: (g b1 -> g b2 -> Ordering)
             -> Joker g a1 b1 -> Joker g a2 b2 -> Ordering
compareJoker = coerce
{-# inline compareJoker #-}

readsPrecJoker :: (Int -> ReadS (g b))
               -> Int -> ReadS (Joker g a b)
readsPrecJoker rpB p =
  readParen (p > 10) $ \s0 -> do
    ("Joker",    s1) <- lex s0
    ("{",        s2) <- lex s1
    ("runJoker", s3) <- lex s2
    (x,          s4) <- rpB 0 s3
    ("}",        s5) <- lex s4
    return (Joker x, s5)

showsPrecJoker :: (Int -> g b -> ShowS)
               -> Int -> Joker g a b -> ShowS
showsPrecJoker spB p (Joker x) =
  showParen (p > 10) $
      showString "Joker {runJoker = "
    . spB 0 x
    . showChar '}'

instance Functor g => Bifunctor (Joker g) where
  first _ = Joker #. runJoker
  {-# inline first #-}
  second g = Joker #. fmap g .# runJoker
  {-# inline second #-}
  bimap _ g = Joker #. fmap g .# runJoker
  {-# inline bimap #-}

instance Functor g => Functor (Joker g a) where
  fmap g = Joker #. fmap g .# runJoker
  {-# inline fmap #-}

instance Applicative g => Biapplicative (Joker g) where
  bipure _ = Joker #. pure
  {-# inline bipure #-}

  (<<*>>) = \ mf -> Joker #. (<*>) (runJoker mf) .# runJoker
  {-# inline (<<*>>) #-}

instance Foldable g => Bifoldable (Joker g) where
  bifoldMap _ g = foldMap g .# runJoker
  {-# inline bifoldMap #-}

instance Foldable g => Foldable (Joker g a) where
  foldMap g = foldMap g .# runJoker
  {-# inline foldMap #-}

instance Traversable g => Bitraversable (Joker g) where
  bitraverse _ g = fmap Joker . traverse g .# runJoker
  {-# inline bitraverse #-}

instance Traversable g => Traversable (Joker g a) where
  traverse g = fmap Joker . traverse g .# runJoker
  {-# inline traverse #-}
