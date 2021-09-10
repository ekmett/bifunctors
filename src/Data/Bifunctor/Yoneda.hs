{-# Language BlockArguments #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
{-# Language Trustworthy #-}
{-# Language DeriveFunctor #-}
{-# Language StandaloneDeriving #-}
{-# Language DerivingStrategies #-}


module Data.Bifunctor.Yoneda
( Yoneda(..)
, Coyoneda(..)
, liftYoneda
, lowerYoneda
, liftCoyoneda
, lowerCoyoneda
) where

import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Bifunctor.Functor
import Data.Foldable
import Data.Function (on)
import Data.Functor.Classes
import Text.Read (Read (..), readListPrecDefault)
import Data.Type.Equality (TestEquality (..))
import Data.Type.Coercion (TestCoercion (..))

newtype Yoneda p a b = Yoneda { runYoneda :: forall x y. (a -> x) -> (b -> y) -> p x y }
  deriving stock Functor

-- TODO: Bifoldable needs a Foldable (p a) superclass

-- Aside from having (mildly) weaker constraints than bireturn and biextract,
-- and helping considerably with inference, liftYoneda and lowerYoneda give us
-- nice names to use for `Show` and `Read`.

-- | A specialized version of 'bireturn'.
liftYoneda :: Bifunctor p => p a b -> Yoneda p a b
liftYoneda pab = Yoneda $ \ax by -> bimap ax by pab

-- | A specialized version of 'biextract'.
lowerYoneda :: Yoneda p a b -> p a b
lowerYoneda (Yoneda f) = f id id

instance Foldable (p a) => Foldable (Yoneda p a) where
  foldMap = \g yo -> fold $ runYoneda yo id g
  {-# inline foldMap #-}

instance (Bifunctor p, Traversable (p a)) => Traversable (Yoneda p a) where
  traverse = \g yo -> fmap liftYoneda $ sequenceA $ runYoneda yo id g
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
  
instance Bitraversable p => Bitraversable (Yoneda p) where
  bitraverse = \f g yo -> liftYoneda <$> bisequence (runYoneda yo f g)
  {-# inline bitraverse #-}

instance Biapplicative p => Biapplicative (Yoneda p) where
  bipure = \a b -> Yoneda \f g -> bipure (f a) (g b)
  {-# inline bipure #-}
  biliftA2 = \f g x y -> Yoneda \f' g' -> 
    biliftA2 (f'.) (g'.) (runYoneda x f g) (biextract y)
  {-# inline biliftA2 #-}
  (<<*>>) = \x y -> Yoneda \f g -> 
    runYoneda x (f.) (g.) <<*>> biextract y
  {-# inline (<<*>>) #-}

instance Eq (p a b) => Eq (Yoneda p a b) where
  (==) = (==) `on` lowerYoneda

instance Eq1 (p a) => Eq1 (Yoneda p a) where
  liftEq eq x y = liftEq eq (lowerYoneda x) (lowerYoneda y)

instance Eq2 p => Eq2 (Yoneda p) where
  liftEq2 f g x y = liftEq2 f g (lowerYoneda x) (lowerYoneda y)

instance Ord (p a b) => Ord (Yoneda p a b) where
  compare = compare `on` lowerYoneda
  -- We can't do anything special for min or max without
  -- a Bifunctor p constraint. Do we want one?

instance Ord1 (p a) => Ord1 (Yoneda p a) where
  liftCompare cmp x y = liftCompare cmp (lowerYoneda x) (lowerYoneda y)

instance Ord2 p => Ord2 (Yoneda p) where
  liftCompare2 f g x y = liftCompare2 f g (lowerYoneda x) (lowerYoneda y)

instance Show (p a b) => Show (Yoneda p a b) where
  showsPrec = showsUnaryWith (\i -> showsPrec i . lowerYoneda) "liftYoneda"

instance Show1 (p a) => Show1 (Yoneda p a) where
  liftShowsPrec sp sl = showsUnaryWith (\i -> liftShowsPrec sp sl i . lowerYoneda) "liftYoneda"

instance Show2 p => Show2 (Yoneda p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 = showsUnaryWith (\i -> liftShowsPrec2 sp1 sl1 sp2 sl2 i . lowerYoneda) "liftYoneda"

instance (Read (p a b), Bifunctor p) => Read (Yoneda p a b) where
  readPrec = readData $ readUnaryWith readPrec "liftYoneda" liftYoneda
  readListPrec = readListPrecDefault

instance (Read1 (p a), Bifunctor p) => Read1 (Yoneda p a) where
  liftReadPrec rp rl = readData $ readUnaryWith (liftReadPrec rp rl) "liftYoneda" liftYoneda
  liftReadListPrec = liftReadListPrecDefault

instance (Read2 p, Bifunctor p) => Read2 (Yoneda p) where
  liftReadPrec2 rp1 rl1 rp2 rl2 = readData $
    readUnaryWith (liftReadPrec2 rp1 rl1 rp2 rl2) "liftYoneda" liftYoneda
  liftReadListPrec2 = liftReadListPrec2Default

instance TestEquality (p a) => TestEquality (Yoneda p a) where
  testEquality x y = testEquality (lowerYoneda x) (lowerYoneda y)

instance TestCoercion (p a) => TestCoercion (Yoneda p a) where
  testCoercion x y = testCoercion (lowerYoneda x) (lowerYoneda y)

-- ----------
-- Coyoneda

data Coyoneda p a b where
  Coyoneda :: (x -> a) -> (y -> b) -> p x y -> Coyoneda p a b

-- | A specialized version of 'bireturn'.
liftCoyoneda :: p a b -> Coyoneda p a b
liftCoyoneda = Coyoneda id id

-- | A specialized version of 'biextract'.
lowerCoyoneda :: Bifunctor p => Coyoneda p a b -> p a b
lowerCoyoneda (Coyoneda f g p) = bimap f g p

instance Functor (Coyoneda p a) where
  fmap f (Coyoneda xa yb pxy) = Coyoneda xa (f . yb) pxy

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

instance Bitraversable p => Bitraversable (Coyoneda p) where
  bitraverse = \f g (Coyoneda h i p) -> liftCoyoneda <$> bitraverse (f . h) (g . i) p
  {-# inline bitraverse #-}

instance (Foldable (p a), Bifunctor p) => Foldable (Coyoneda p a) where
  foldMap f (Coyoneda xa yb pxy) = fold (bimap xa (f . yb) pxy)

instance (Traversable (p a), Bifunctor p) => Traversable (Coyoneda p a) where
  traverse f (Coyoneda xa yb pxy) =
    fmap liftCoyoneda $ sequenceA (bimap xa (f . yb) pxy)

instance Biapplicative p => Biapplicative (Coyoneda p) where
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

instance (Eq (p a b), Bifunctor p) => Eq (Coyoneda p a b) where
  (==) = (==) `on` lowerCoyoneda

instance (Eq1 (p a), Bifunctor p) => Eq1 (Coyoneda p a) where
  liftEq eq x y = liftEq eq (lowerCoyoneda x) (lowerCoyoneda y)

instance (Eq2 p, Bifunctor p) => Eq2 (Coyoneda p) where
  liftEq2 f g x y = liftEq2 f g (lowerCoyoneda x) (lowerCoyoneda y)

instance (Ord (p a b), Bifunctor p) => Ord (Coyoneda p a b) where
  compare = compare `on` lowerCoyoneda
  -- This min leans on the underlying instance, which is nice, but
  -- it also unconditionally rewraps, which isn't so nice if
  -- the underlying instance just does a plain compare and choose.
  -- I think it's still a reasonable choice, since Coyoneda may
  -- be used in certain lazy situations where the underlying instance
  -- may be important.
  min x y = liftCoyoneda $ (min `on` lowerCoyoneda) x y
  max x y = liftCoyoneda $ (max `on` lowerCoyoneda) x y

instance (Ord1 (p a), Bifunctor p) => Ord1 (Coyoneda p a) where
  liftCompare cmp x y = liftCompare cmp (lowerCoyoneda x) (lowerCoyoneda y)

instance (Ord2 p, Bifunctor p) => Ord2 (Coyoneda p) where
  liftCompare2 f g x y = liftCompare2 f g (lowerCoyoneda x) (lowerCoyoneda y)

instance (Show (p a b), Bifunctor p) => Show (Coyoneda p a b) where
  showsPrec = showsUnaryWith (\i -> showsPrec i . lowerCoyoneda) "liftCoyoneda"

instance (Show1 (p a), Bifunctor p) => Show1 (Coyoneda p a) where
  liftShowsPrec sp sl = showsUnaryWith (\i -> liftShowsPrec sp sl i . lowerCoyoneda) "liftCoyoneda"

instance (Show2 p, Bifunctor p) => Show2 (Coyoneda p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 = showsUnaryWith (\i -> liftShowsPrec2 sp1 sl1 sp2 sl2 i . lowerCoyoneda) "liftCoyoneda"

instance Read (p a b) => Read (Coyoneda p a b) where
  readPrec = readData $ readUnaryWith readPrec "liftCoyoneda" liftCoyoneda
  readListPrec = readListPrecDefault

instance Read1 (p a) => Read1 (Coyoneda p a) where
  liftReadPrec rp rl = readData $ readUnaryWith (liftReadPrec rp rl) "liftCoyoneda" liftCoyoneda
  liftReadListPrec = liftReadListPrecDefault

instance Read2 p => Read2 (Coyoneda p) where
  liftReadPrec2 rp1 rl1 rp2 rl2 = readData $ readUnaryWith (liftReadPrec2 rp1 rl1 rp2 rl2) "liftCoyoneda" liftCoyoneda
  liftReadListPrec2 = liftReadListPrec2Default

instance (TestEquality (p a), Bifunctor p) => TestEquality (Coyoneda p a) where
  testEquality x y = testEquality (lowerCoyoneda x) (lowerCoyoneda y)

instance (TestCoercion (p a), Bifunctor p) => TestCoercion (Coyoneda p a) where
  testCoercion x y = testCoercion (lowerCoyoneda x) (lowerCoyoneda y)
