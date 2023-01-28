{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Unsafe #-}

{-|
Module:      Data.Bifunctor.TH.Internal
Copyright:   (C) 2008-2016 Edward Kmett, (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Edward Kmett
Portability: Template Haskell

Template Haskell-related utilities.
-}
module Data.Bifunctor.TH.Internal where

import           Control.Applicative
import           Data.Bifunctor (Bifunctor(..))
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bitraversable (Bitraversable(..))
import           Data.Coerce (coerce)
import           Data.Foldable (foldr')
import qualified Data.List as List
import qualified Data.Map as Map (singleton)
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid (Dual(..), Endo(..))
import qualified Data.Set as Set
import           Data.Set (Set)

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

applySubstitutionKind :: Map Name Kind -> Type -> Type
applySubstitutionKind = applySubstitution

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (Map.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (flip substNameWithKind starK) t ns

-------------------------------------------------------------------------------
-- Type-specialized const functions
-------------------------------------------------------------------------------

bimapConst :: p b d -> (a -> b) -> (c -> d) -> p a c -> p b d
bimapConst = const . const . const
{-# INLINE bimapConst #-}

bifoldrConst :: c -> (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
bifoldrConst = const . const . const . const
{-# INLINE bifoldrConst #-}

bifoldMapConst :: m -> (a -> m) -> (b -> m) -> p a b -> m
bifoldMapConst = const . const . const
{-# INLINE bifoldMapConst #-}

bitraverseConst :: f (t c d) -> (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bitraverseConst = const . const . const
{-# INLINE bitraverseConst #-}

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t
  | hasKindStar t = KindStar
  | otherwise = case t of
                     SigT _ (VarT k) -> IsKindVar k
                     _               -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- filterByList, filterByLists, and partitionByList taken from GHC (BSD3-licensed)

-- | 'filterByList' takes a list of Bools and a list of some elements and
-- filters out these elements for which the corresponding value in the list of
-- Bools is False. This function does not check whether the lists have equal
-- length.
filterByList :: [Bool] -> [a] -> [a]
filterByList (True:bs)  (x:xs) = x : filterByList bs xs
filterByList (False:bs) (_:xs) =     filterByList bs xs
filterByList _          _      = []

-- | 'filterByLists' takes a list of Bools and two lists as input, and
-- outputs a new list consisting of elements from the last two input lists. For
-- each Bool in the list, if it is 'True', then it takes an element from the
-- former list. If it is 'False', it takes an element from the latter list.
-- The elements taken correspond to the index of the Bool in its list.
-- For example:
--
-- @
-- filterByLists [True, False, True, False] \"abcd\" \"wxyz\" = \"axcz\"
-- @
--
-- This function does not check whether the lists have equal length.
filterByLists :: [Bool] -> [a] -> [a] -> [a]
filterByLists (True:bs)  (x:xs) (_:ys) = x : filterByLists bs xs ys
filterByLists (False:bs) (_:xs) (y:ys) = y : filterByLists bs xs ys
filterByLists _          _      _      = []

-- | 'partitionByList' takes a list of Bools and a list of some elements and
-- partitions the list according to the list of Bools. Elements corresponding
-- to 'True' go to the left; elements corresponding to 'False' go to the right.
-- For example, @partitionByList [True, False, True] [1,2,3] == ([1,3], [2])@
-- This function does not check whether the lists have equal
-- length.
partitionByList :: [Bool] -> [a] -> ([a], [a])
partitionByList = go [] []
  where
    go trues falses (True  : bs) (x : xs) = go (x:trues) falses bs xs
    go trues falses (False : bs) (x : xs) = go trues (x:falses) bs xs
    go trues falses _ _ = (reverse trues, reverse falses)

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
hasKindStar (SigT _ StarT) = True
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
isStarOrVar _      = False

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (freeVariables uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | A mapping of type variable Names to their map function Names. For example, in a
-- Bifunctor declaration, a TyVarMap might look like (a ~> f, b ~> g), where
-- a and b are the last two type variables of the datatype, and f and g are the two
-- functions which map their respective type variables.
type TyVarMap = Map Name Name

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

unsnoc :: [a] -> Maybe ([a], a)
unsnoc []     = Nothing
unsnoc (x:xs) = case unsnoc xs of
                  Nothing    -> Just ([], x)
                  Just (a,b) -> Just (x:a, b)

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Applies a typeclass constraint to a type.
applyClass :: Name -> Name -> Pred
applyClass con t = AppT (ConT con) (VarT t)

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToName_maybe :: Type -> Maybe Name
varTToName_maybe (VarT n)   = Just n
varTToName_maybe (SigT t _) = varTToName_maybe t
varTToName_maybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToName_maybe

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Detect if a Name in a list of provided Names occurs as an argument to some
-- type family. This makes an effort to exclude /oversaturated/ arguments to
-- type families. For instance, if one declared the following type family:
--
-- @
-- type family F a :: Type -> Type
-- @
--
-- Then in the type @F a b@, we would consider @a@ to be an argument to @F@,
-- but not @b@.
isInTypeFamilyApp :: [Name] -> Type -> [Type] -> Q Bool
isInTypeFamilyApp names tyFun tyArgs =
  case tyFun of
    ConT tcName -> go tcName
    _           -> return False
  where
    go :: Name -> Q Bool
    go tcName = do
      info <- reify tcName
      case info of
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> withinFirstArgs bndrs
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> withinFirstArgs bndrs
        _ -> return False
      where
        withinFirstArgs :: [a] -> Q Bool
        withinFirstArgs bndrs =
          let firstArgs = take (length bndrs) tyArgs
              argFVs    = freeVariables firstArgs
          in return $ any (`elem` argFVs) names

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t k)   names = go t  names || go k  names
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
predMentionsName = mentionsName

-- | Construct a type via curried application.
applyTy :: Type -> [Type] -> Type
applyTy = List.foldl' AppT

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = applyTy . ConT

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> (Type, [Type])
unapplyTy ty = go ty ty []
  where
    go :: Type -> Type -> [Type] -> (Type, [Type])
    go _      (AppT ty1 ty2)     args = go ty1 ty1 (ty2:args)
    go origTy (SigT ty' _)       args = go origTy ty' args
    go origTy (InfixT ty1 n ty2) args = go origTy (ConT n `AppT` ty1 `AppT` ty2) args
    go origTy (ParensT ty')      args = go origTy ty' args
    go origTy _                  args = (origTy, args)

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a ~ b) => (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- (a ~ b, [a -> b, Char, ()])
-- @
uncurryTy :: Type -> (Cxt, [Type])
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1:tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], [t])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> [Kind]
uncurryKind = snd . uncurryTy

-------------------------------------------------------------------------------
-- Quoted names
-------------------------------------------------------------------------------

bimapConstValName :: Name
bimapConstValName = 'bimapConst

bifoldrConstValName :: Name
bifoldrConstValName = 'bifoldrConst

bifoldMapConstValName :: Name
bifoldMapConstValName = 'bifoldMapConst

coerceValName :: Name
coerceValName = 'coerce

bitraverseConstValName :: Name
bitraverseConstValName = 'bitraverseConst

wrapMonadDataName :: Name
wrapMonadDataName = 'WrapMonad

functorTypeName :: Name
functorTypeName = ''Functor

foldableTypeName :: Name
foldableTypeName = ''Foldable

traversableTypeName :: Name
traversableTypeName = ''Traversable

composeValName :: Name
composeValName = '(.)

idValName :: Name
idValName = 'id

errorValName :: Name
errorValName = 'error

flipValName :: Name
flipValName = 'flip

fmapValName :: Name
fmapValName = 'fmap

foldrValName :: Name
foldrValName = 'foldr

foldMapValName :: Name
foldMapValName = 'foldMap

seqValName :: Name
seqValName = 'seq

traverseValName :: Name
traverseValName = 'traverse

unwrapMonadValName :: Name
unwrapMonadValName = 'unwrapMonad

bifunctorTypeName :: Name
bifunctorTypeName = ''Bifunctor

bimapValName :: Name
bimapValName = 'bimap

pureValName :: Name
pureValName = 'pure

apValName :: Name
apValName = '(<*>)

liftA2ValName :: Name
liftA2ValName = 'liftA2

mappendValName :: Name
mappendValName = 'mappend

memptyValName :: Name
memptyValName = 'mempty

bifoldableTypeName :: Name
bifoldableTypeName = ''Bifoldable

bitraversableTypeName :: Name
bitraversableTypeName = ''Bitraversable

bifoldrValName :: Name
bifoldrValName = 'bifoldr

bifoldMapValName :: Name
bifoldMapValName = 'bifoldMap

bitraverseValName :: Name
bitraverseValName = 'bitraverse

appEndoValName :: Name
appEndoValName = 'appEndo

dualDataName :: Name
dualDataName = 'Dual

endoDataName :: Name
endoDataName = 'Endo

getDualValName :: Name
getDualValName = 'getDual
