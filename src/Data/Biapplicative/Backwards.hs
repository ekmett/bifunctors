{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright   :  (C) 2021 David Feuer
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :
--
-- 'Biapplicative's, backwards.

module Data.Biapplicative.Backwards
( Backwards (..)
) where

import Control.Applicative (Alternative)
import Data.Biapplicative
import Data.Coerce
import qualified Data.Bifunctor as Base
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import GHC.Generics (Generic, Generic1)
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

-- | An analogue of @"Control.Applicative.Backwards".'Control.Applicative.Backwards.Backwards'@
-- for bifunctors. The 'Biapplicative' instance performs actions
-- in the reverse order. All other instances are essentially derived ones.
--
-- @
-- 'bipure' a b = Backwards ('bipure' a b)
-- 'biliftA2' f g (Backwards m) (Backwards n) = Backwards $ 'biliftA2' ('flip' f) ('flip' g) n m
-- @
newtype Backwards p a b = Backwards { forwards :: p a b }
  deriving stock (Traversable, Generic, Generic1, Data)
  deriving newtype ( Eq, Ord, Functor, Foldable, Base.Bifunctor, Bifoldable
                   , Semigroup, Monoid, Applicative, Alternative, Monad, MonadFix
                   , MonadPlus, MonadFail, Contravariant, TestEquality
                   , Eq1, Eq2, Ord1, Ord2 )

instance Show (p a b) => Show (Backwards p a b) where
  showsPrec d (Backwards p) =
    showsUnaryWith showsPrec "Backwards" d p

instance Show2 p => Show2 (Backwards p) where
  liftShowsPrec2 spa sla spb slb d (Backwards p) =
    showsUnaryWith (liftShowsPrec2 spa sla spb slb) "Backwards" d p

instance Show1 (p a) => Show1 (Backwards p a) where
  liftShowsPrec sp sl d (Backwards p) =
    showsUnaryWith (liftShowsPrec sp sl) "Backwards" d p

-- | Accepts either plain or record syntax.
instance Read (p a b) => Read (Backwards p a b) where
  readPrec = liftReadPrecBackwards TR.readPrec
  readList = TR.readListDefault
  readListPrec = TR.readListPrecDefault

-- | Accepts either plain or record syntax.
instance Read1 (p a) => Read1 (Backwards p a) where
  liftReadPrec rp rl =
    liftReadPrecBackwards (liftReadPrec rp rl)
  liftReadListPrec = liftReadListPrecDefault

-- | Accepts either plain or record syntax.
instance Read2 p => Read2 (Backwards p) where
  liftReadPrec2 rp1 rl1 rp2 rl2 =
    liftReadPrecBackwards (liftReadPrec2 rp1 rl1 rp2 rl2)
  liftReadListPrec2 = liftReadListPrec2Default

-- Trivially unifies the implementations of Read, Read1, and Read2
liftReadPrecBackwards :: TR.ReadPrec (p a b) -> TR.ReadPrec (Backwards p a b)
liftReadPrecBackwards read_p =
    TR.parens $ do
      TRL.Ident "Backwards" <- TR.lexP
      (TPR.prec 11 $ do
         TRL.Punc "{" <- TR.lexP
         TRL.Ident "forwards" <- TR.lexP
         TRL.Punc "=" <- TR.lexP
         p <- read_p
         TRL.Punc "}" <- TR.lexP
         pure (Backwards p))
        TR.+++
          (TPR.prec 10 $ do
             p <- TR.step $ read_p
             pure (Backwards p))

instance Biapplicative p => Biapplicative (Backwards p) where
  bipure :: forall a b. a -> b -> Backwards p a b
  bipure = coerce (bipure @p @a @b)

  biliftA2 f g (Backwards m) (Backwards n) = Backwards $ biliftA2 (flip f) (flip g) n m

instance Bitraversable p => Bitraversable (Backwards p) where
  bitraverse f g (Backwards m) = Backwards <$> bitraverse f g m
