{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:      T124Spec
-- Copyright:   (C) 2023 Ellie Hermaszewska
-- License:     BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Portability: Template Haskell
--
-- A regression test for https://github.com/ekmett/bifunctors/issues/124
-- which ensures that the TH deriver doesn't generate ill-kinded code
module T124Spec where

import Data.Bifunctor.TH
import Test.Hspec

data Bar a b where
  Bar :: b -> Bar 'True b

deriving instance Functor (Bar a)
deriving instance Foldable (Bar a)
deriving instance Traversable (Bar a)

data Foo b c where
  -- Param 2 on the right with no bifunctor instance
  Foo1 :: Bar x c -> Foo b c
  -- Param 1 on the right with no bifunctor instance
  Foo2 :: Bar x b -> Foo b c

deriveBifunctor ''Foo
deriveBifoldable ''Foo
deriveBitraversable ''Foo

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
