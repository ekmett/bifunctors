{-# LANGUAGE Unsafe #-}
module Data.Bifunctor.Unsafe where

import Data.Coerce

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
infixr 9 #.

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f
infixl 8 .#
