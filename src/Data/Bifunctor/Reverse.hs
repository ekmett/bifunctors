{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright   :  (C) 2021 David Feuer
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :
--
-- Making bifunctors whose elements are notionally in the
-- reverse order from the original bifunctor.

module Data.Bifunctor.Reverse
  ( Reverse (..)
  ) where

import Control.Applicative (Alternative)
import Data.Biapplicative
import qualified Data.Bifunctor as Base
import GHC.Generics (Generic, Generic1)
import qualified Data.Functor.Reverse as FunReverse
import Control.Applicative.Backwards
import Data.Coerce
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Semigroup (Dual (..))
import Data.Functor.Classes
import qualified Text.ParserCombinators.ReadPrec as TPR
import qualified Text.Read.Lex as TRL
import qualified Text.Read as TR
import Control.Monad (MonadPlus)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Data.Functor.Contravariant (Contravariant)
import Data.Type.Equality (TestEquality)
import Data.Data (Data)

-- | The same bifunctor, but with `Bifoldable`, `Bitraversable`,
-- `Foldable` and `Traversable` instances that process the elements
-- in the reverse order. All other instances are essentially derived
-- ones.
--
-- @
-- 'bitraverse'
--   (\c -> do print c; (,) c <$> (readLn :: IO Int))
--   (\b -> do print b; pure b)
--   (Reverse $ "Data.Bifunctor.Tannen".'Data.Bifunctor.Tannen.Tannen' [Left 'a', Right False, Left 'q'])
--
-- 'q' -- output
-- 12  -- input
-- False -- output
-- 'a' -- output
-- 13 -- input
-- Reverse ('Data.Bifunctor.Tannen.Tannen' {runTannen = [Left ('a',13),Right False,Left ('q',12)]}) -- output
-- @
newtype Reverse t a b = Reverse { getReverse :: t a b }
  deriving stock (Generic, Generic1, Data)
  deriving Foldable via FunReverse.Reverse (t a)

  deriving newtype ( Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix
                   , MonadFail, Contravariant, TestEquality
                   , Eq, Eq1, Eq2, Ord, Ord1, Ord2
                   , Base.Bifunctor, Biapplicative, Semigroup, Monoid )

instance Bifoldable t => Bifoldable (Reverse t) where
  bifoldMap f g (Reverse t) = getDual $ bifoldMap (coerce f) (coerce g) t
  bifoldr c1 c2 n (Reverse t) = bifoldl (flip c1) (flip c2) n t
  bifoldl c1 c2 b (Reverse t) = bifoldr (flip c1) (flip c2) b t
  -- We can't do anything special for bifold.

instance Bitraversable t => Bitraversable (Reverse t) where
  bitraverse f g (Reverse t) = fmap Reverse . forwards $ bitraverse (coerce f) (coerce g) t

instance Traversable (t a) => Traversable (Reverse t a) where
  traverse f (Reverse t) = fmap Reverse . forwards $ traverse (coerce f) t

instance Show (p a b) => Show (Reverse p a b) where
  showsPrec d (Reverse p) =
    showsUnaryWith showsPrec "Reverse" d p

instance Show2 p => Show2 (Reverse p) where
  liftShowsPrec2 spa sla spb slb d (Reverse p) =
    showsUnaryWith (liftShowsPrec2 spa sla spb slb) "Reverse" d p

instance Show1 (p a) => Show1 (Reverse p a) where
  liftShowsPrec sp sl d (Reverse p) =
    showsUnaryWith (liftShowsPrec sp sl) "Reverse" d p

-- | Accepts either plain or record syntax.
instance Read (p a b) => Read (Reverse p a b) where
  readPrec = liftReadPrecReverse TR.readPrec
  readList = TR.readListDefault
  readListPrec = TR.readListPrecDefault

-- | Accepts either plain or record syntax.
instance Read1 (p a) => Read1 (Reverse p a) where
  liftReadPrec rp rl =
    liftReadPrecReverse (liftReadPrec rp rl)
  liftReadListPrec = liftReadListPrecDefault

-- | Accepts either plain or record syntax.
instance Read2 p => Read2 (Reverse p) where
  liftReadPrec2 rp1 rl1 rp2 rl2 =
    liftReadPrecReverse (liftReadPrec2 rp1 rl1 rp2 rl2)
  liftReadListPrec2 = liftReadListPrec2Default

-- Trivially unifies the implementations of Read, Read1, and Read2
liftReadPrecReverse :: TR.ReadPrec (p a b) -> TR.ReadPrec (Reverse p a b)
liftReadPrecReverse read_p =
    TR.parens $ do
      TRL.Ident "Reverse" <- TR.lexP
      (TPR.prec 11 $ do
         TRL.Punc "{" <- TR.lexP
         TRL.Ident "getReverse" <- TR.lexP
         TRL.Punc "=" <- TR.lexP
         p <- read_p
         TRL.Punc "}" <- TR.lexP
         pure (Reverse p))
        TR.+++
          (TPR.prec 10 $ do
             p <- TR.step $ read_p
             pure (Reverse p))

