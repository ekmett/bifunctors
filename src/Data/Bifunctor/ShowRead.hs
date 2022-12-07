{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language Safe #-}

-- | Types for lifting instances of `Show`N and `Read`N for record newtypes. We
-- don't show record syntax, because it's too much clutter, but we accept it
-- when reading.
--
-- When @a@ is a newtype (or close enough) /defined using record syntax/, and
-- is an instance of 'Generic', 'Show', and 'Read',  @'ShowRead' a@ is a
-- @DerivingVia@ target implementing a plain 'Show' instance and a flexible
-- 'Read' instance. This is a fairly specific situation, but it pops up all
-- over this package. We could pretty easily expand to non-record types if
-- we needed to.
module Data.Bifunctor.ShowRead
  ( ShowRead (..)
  , ShowRead1 (..)
  , ShowRead2 (..)
  , liftReadPrecWhatever
  , liftShowsPrecWhatever
  ) where
import qualified Text.ParserCombinators.ReadPrec as TPR
import qualified Text.Read.Lex as TRL
import qualified Text.Read as TR
import Text.Read (ReadPrec, Read (..), readListPrecDefault)
import GHC.Generics
import Data.Kind
import Data.Functor.Classes

newtype ShowRead a = ShowRead a
newtype ShowRead1 f a = ShowRead1 (f a)
newtype ShowRead2 f a b = ShowRead2 (f a b)

instance (Wraps n d c s o, Read o) => Read (ShowRead n) where
  readPrec = ShowRead <$> liftReadPrecWhatever readPrec
  readListPrec = readListPrecDefault

instance (Wraps n d c s o, Show o) => Show (ShowRead n) where
  showsPrec d (ShowRead x) = liftShowsPrecWhatever showsPrec d x

instance (Wraps1 n d c s o, Read1 o) => Read1 (ShowRead1 n) where
  liftReadPrec rp rl = ShowRead1 <$> liftReadPrecWhatever @(n _) @d @c @s @(o _) (liftReadPrec rp rl)
  liftReadListPrec = liftReadListPrecDefault

instance (Wraps1 n f c s o, Read1 o, Read a) => Read (ShowRead1 n a) where
  readPrec = readPrec1
  readListPrec = readListPrecDefault

instance (Wraps1 n d c s o, Show1 o) => Show1 (ShowRead1 n) where
  liftShowsPrec sp sl d (ShowRead1 x) = liftShowsPrecWhatever @(n _) @d @c @s @(o _) (liftShowsPrec sp sl) d x

instance (Wraps1 n d c s o, Show1 o, Show a) => Show (ShowRead1 n a) where
  showsPrec = showsPrec1

instance (Wraps2 n d c s o, Read2 o) => Read2 (ShowRead2 n) where
  liftReadPrec2 rp1 rl1 rp2 rl2 = ShowRead2 <$> liftReadPrecWhatever @(n _ _) @d @c @s @(o _ _) (liftReadPrec2 rp1 rl1 rp2 rl2)
  liftReadListPrec2 = liftReadListPrec2Default

instance (Wraps2 n f c s o, Read2 o, Read a, Read b) => Read (ShowRead2 n a b) where
  readPrec = readPrec2
  readListPrec = readListPrecDefault

instance (Wraps2 n f c s o, Read2 o, Read a) => Read1 (ShowRead2 n a) where
  liftReadPrec = liftReadPrec2 readPrec readListPrec
  liftReadListPrec = liftReadListPrecDefault

instance (Wraps2 n d c s o, Show2 o) => Show2 (ShowRead2 n) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (ShowRead2 x) =
    liftShowsPrecWhatever @(n _ _) @d @c @s @(o _ _) (liftShowsPrec2 sp1 sl1 sp2 sl2) d x

instance (Wraps2 n d c s o, Show2 o, Show a, Show b) => Show (ShowRead2 n a b) where
  showsPrec = showsPrec2

instance (Wraps2 n d c s o, Show2 o, Show a) => Show1 (ShowRead2 n a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

type WrapsF n d c s o =
  ( Generic n
  , Rep n ~ D1 d (C1 c (S1 s (Rec0 o)))
  , Constructor c
  , Selector s )

class WrapsF n d c s o => Wraps n d c s o
instance WrapsF n d c s o => Wraps n d c s o

type family Any where

class (forall a. Wraps (n a) d c s (o a), Wraps (n Any) d c s (o Any)) => Wraps1 n d c s o
instance (forall a. Wraps (n a) d c s (o a)) => Wraps1 n d c s o

class (forall a b. Wraps (n a b) d c s (o a b), Wraps (n Any Any) d c s (o Any Any)) => Wraps2 n d c s o
instance (forall a b. Wraps (n a b) d c s (o a b)) => Wraps2 n d c s o

data Prox (c :: Meta) (f :: Type -> Type) a = Prox

-- | Given a way to read the underlying type of a newtype
-- or similar, produce a way to read the newtype itself
-- using either record syntax or plain syntax.
liftReadPrecWhatever
  :: forall n d c s o.
     ( Generic n
     , Wraps n d c s o
     , Constructor c
     , Selector s)
  => ReadPrec o -> ReadPrec n
liftReadPrecWhatever read_p =
    TR.parens $ do
      expectP (TRL.Ident $ conName (Prox @c))
      (TPR.prec 11 $ do
         expectP (TRL.Punc "{")
         expectP (TRL.Ident $ selName (Prox @s))
         expectP (TRL.Punc "=")
         p <- read_p
         expectP (TRL.Punc "}")
         pure (to (M1 (M1 (M1 (K1 p))))))
        TR.+++
          (TPR.prec 10 $ do
             p <- TR.step $ read_p
             pure (to (M1 (M1 (M1 (K1 p))))))

-- Copied from GHC.Read
expectP :: TRL.Lexeme -> ReadPrec ()
expectP lexeme = TR.lift (TRL.expect lexeme)

-- | Given a way to show the type wrapped by a newtype,
-- produce a way to show the newtype in plain syntax.
liftShowsPrecWhatever
  :: forall n d c s o.
     ( Generic n
     , Wraps n d c s o
     , Constructor c )
  => (Int -> o -> ShowS) -> Int -> n -> ShowS
liftShowsPrecWhatever sp d n = showsUnaryWith sp (conName (Prox @c)) d (unK1 (unM1 (unM1 (unM1 (from n)))))
