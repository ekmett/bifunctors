{-# Language BlockArguments #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
{-# Language Safe #-}
{-# Language TypeOperators #-}
{-# Language DeriveFunctor #-}
{-# Language StandaloneDeriving #-}
{-# Language QuantifiedConstraints #-}
{-# Language DerivingStrategies #-}

module Data.Bifunctor.Yoneda
( Yoneda(..)
, Coyoneda(..)
) where

import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor.Classes
import Data.Bifunctor.Functor
import Data.Foldable

newtype Yoneda p a b = Yoneda { runYoneda :: forall x y. (a -> x) -> (b -> y) -> p x y }
  deriving stock Functor

-- TODO: Bifoldable needs a Foldable (p a) superclass

instance Foldable (p a) => Foldable (Yoneda p a) where
  foldMap = \g yo -> fold $ runYoneda yo id g
  {-# inline foldMap #-}

instance (Bifunctor' p, Traversable (p a)) => Traversable (Yoneda p a) where
  traverse = \g yo -> fmap bireturn $ sequenceA $ runYoneda yo id g
  {-# inline traverse #-}

instance Bifunctor (Yoneda p) where
  bimap = \aa' bb' k -> Yoneda \a'x b'y -> runYoneda k (a'x . aa') (b'y . bb')
  first = \aa' k -> Yoneda \a'x b'y -> runYoneda k (a'x . aa') b'y
  second = \bb' k -> Yoneda \a'x b'y -> runYoneda k a'x (b'y . bb')
  {-# inline bimap #-}
  {-# inline first #-}
  {-# inline second #-}

instance BifunctorFunctor Yoneda where
  bifmap = \f p -> Yoneda \g h -> f (runYoneda p g h)
  {-# inline bifmap #-}

instance BifunctorMonad Yoneda where
  bireturn = \p -> Yoneda \f g -> bimap f g p
  {-# inline bireturn #-}
  bijoin = biextract
  {-# inline bijoin #-}

instance BifunctorComonad Yoneda where
  biextract = \k -> runYoneda k id id 
  {-# inline biextract #-}
  biduplicate = \yo -> Yoneda \f g -> bimap f g yo
  {-# inline biduplicate #-}

instance Bifoldable p => Bifoldable (Yoneda p) where
  bifoldMap = \f g yo -> bifold $ runYoneda yo f g
  {-# inline bifoldMap #-}
  
instance Bitraversable' p => Bitraversable (Yoneda p) where
  bitraverse = \f g yo -> bireturn <$> bisequence (runYoneda yo f g)
  {-# inline bitraverse #-}

instance Biapplicative' p => Biapplicative (Yoneda p) where
  bipure = \a b -> Yoneda \f g -> bipure (f a) (g b)
  {-# inline bipure #-}
  biliftA2 = \f g x y -> Yoneda \f' g' -> 
    biliftA2 (f'.) (g'.) (runYoneda x f g) (biextract y)
  {-# inline biliftA2 #-}
  (<<*>>) = \x y -> Yoneda \f g -> 
    runYoneda x (f.) (g.) <<*>> biextract y
  {-# inline (<<*>>) #-}

data Coyoneda p a b where
  Coyoneda :: (x -> a) -> (y -> b) -> p x y -> Coyoneda p a b

deriving stock instance (forall x. Functor (p x)) => Functor (Coyoneda p a)

instance Bifunctor (Coyoneda p) where
  bimap = \f g (Coyoneda h i p) -> Coyoneda (f . h) (g . i) p
  first = \f (Coyoneda h i p) -> Coyoneda (f . h) i p
  second = \g (Coyoneda h i p) -> Coyoneda h (g . i) p
  {-# inline bimap #-}
  {-# inline first #-}
  {-# inline second #-}

instance Bifoldable p => Bifoldable (Coyoneda p) where
  bifoldMap = \f g (Coyoneda h i p) -> bifoldMap (f . h) (g . i) p
  {-# inline bifoldMap #-}

instance Bitraversable' p => Bitraversable (Coyoneda p) where
  bitraverse = \f g (Coyoneda h i p) -> bireturn <$> bitraverse (f . h) (g . i) p
  {-# inline bitraverse #-}

instance Biapplicative' p => Biapplicative (Coyoneda p) where
  bipure a b = bireturn (bipure a b)
  {-# inline bipure #-}
  biliftA2 = \f g (Coyoneda h i p) (Coyoneda j k q) -> 
    bireturn $ biliftA2 
      (\x x1 -> f (h x) (j x1)) 
      (\y y1 -> g (i y) (k y1))
      p 
      q
  {-# inline biliftA2 #-}
  (<<*>>) = \(Coyoneda h i p) (Coyoneda j k q) -> 
    bireturn $ biliftA2 
      (\x x1 -> h x (j x1)) 
      (\y y1 -> i y (k y1))
      p 
      q
  {-# inline (<<*>>) #-}

instance BifunctorFunctor Coyoneda where
  bifmap f (Coyoneda h i p) = Coyoneda h i (f p)
  {-# inline bifmap #-}

instance BifunctorMonad Coyoneda where
  bireturn = Coyoneda id id
  bijoin = \(Coyoneda f g (Coyoneda h i p)) -> Coyoneda (f . h) (g . i) p
  {-# inline bireturn #-}
  {-# inline bijoin #-}

instance BifunctorComonad Coyoneda where
  biextract (Coyoneda f g p) = bimap f g p
  biduplicate = bireturn
  {-# inline biextract #-}
  {-# inline biduplicate #-}

