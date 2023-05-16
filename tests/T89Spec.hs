{-# LANGUAGE TemplateHaskell #-}

--|
-- Module:      Spec
-- Copyright:   (C) 2008-2023 Edward Kmett, (C) 2015 Ryan Scott
-- License:     BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Portability: Template Haskell
--
-- A regression test for #89 which ensures that a TH-generated Bifoldable
-- instance of a certain shape does not trigger -Wunused-matches warnings.

module T89Spec where

import Data.Bifunctor.TH
import Test.Hspec

data X = MkX
data Y a b = MkY a b
newtype XY a b = XY { getResp :: Either X (Y a b) }

$(deriveBifoldable ''Y)
$(deriveBifoldable ''XY)

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
