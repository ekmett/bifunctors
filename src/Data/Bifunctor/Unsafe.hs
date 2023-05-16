{-# LANGUAGE Unsafe #-}

-- |
-- Copyright   :  (C) 2021-2023 Edward Kmett
-- License     :  BSD-2-Clause OR Apache-2.0
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable

module Data.Bifunctor.Unsafe where

import Data.Coerce

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
infixr 9 #.

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
infixl 8 .#
