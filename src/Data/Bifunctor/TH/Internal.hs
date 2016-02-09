{-# LANGUAGE CPP #-}

{-|
Module:      Data.Bifunctor.TH.Internal
Copyright:   (C) 2008-2016 Edward Kmett, (C) 2015-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Edward Kmett
Portability: Template Haskell

Template Haskell-related utilities.
-}
module Data.Bifunctor.TH.Internal where

import           Control.Monad (liftM)

import           Data.Foldable (foldr')
import           Data.List
import qualified Data.Map as Map (fromList, findWithDefault, singleton)
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

#ifndef CURRENT_PACKAGE_KEY
import           Data.Version (showVersion)
import           Paths_bifunctors (version)
#endif

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

-- | Expands all type synonyms in a type. Written by Dan Rosén in the
-- @genifunctors@ package (licensed under BSD3).
expandSyn :: Type -> Q Type
expandSyn (ForallT tvs ctx t) = fmap (ForallT tvs ctx) $ expandSyn t
expandSyn t@AppT{}            = expandSynApp t []
expandSyn t@ConT{}            = expandSynApp t []
expandSyn (SigT t k)          = do t' <- expandSyn t
                                   k' <- expandSynKind k
                                   return (SigT t' k')
expandSyn t                   = return t

expandSynKind :: Kind -> Q Kind
#if MIN_VERSION_template_haskell(2,8,0)
expandSynKind = expandSyn
#else
expandSynKind = return -- There are no kind synonyms to deal with
#endif

expandSynApp :: Type -> [Type] -> Q Type
expandSynApp (AppT t1 t2) ts = do
    t2' <- expandSyn t2
    expandSynApp t1 (t2':ts)
expandSynApp (ConT n) ts | nameBase n == "[]" = return $ foldl' AppT ListT ts
expandSynApp t@(ConT n) ts = do
    info <- reify n
    case info of
        TyConI (TySynD _ tvs rhs) ->
            let (ts', ts'') = splitAt (length tvs) ts
                subs = mkSubst tvs ts'
                rhs' = substType subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type TypeSubst = Map Name Type
type KindSubst = Map Name Kind

mkSubst :: [TyVarBndr] -> [Type] -> TypeSubst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v)    = v
       un (KindedTV v _) = v
   in Map.fromList $ zip vs' ts

substType :: TypeSubst -> Type -> Type
substType subs (ForallT v c t) = ForallT v c $ substType subs t
substType subs t@(VarT n)      = Map.findWithDefault t n subs
substType subs (AppT t1 t2)    = AppT (substType subs t1) (substType subs t2)
substType subs (SigT t k)      = SigT (substType subs t)
#if MIN_VERSION_template_haskell(2,8,0)
                                      (substType subs k)
#else
                                      k
#endif
substType _ t                  = t

substKind :: KindSubst -> Type -> Type
#if MIN_VERSION_template_haskell(2,8,0)
substKind = substType
#else
substKind _ = id -- There are no kind variables!
#endif

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = substKind (Map.singleton n k)

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
#if MIN_VERSION_template_haskell(2,8,0)
                     SigT _ (VarT k) -> IsKindVar k
#endif
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

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
#if MIN_VERSION_template_haskell(2,8,0)
hasKindStar (SigT _ StarT) = True
#else
hasKindStar (SigT _ StarK) = True
#endif
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
#if MIN_VERSION_template_haskell(2,8,0)
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
#else
isStarOrVar StarK  = True
#endif
isStarOrVar _      = False

-- | Gets all of the type/kind variable names mentioned somewhere in a Type.
tyVarNamesOfType :: Type -> [Name]
tyVarNamesOfType = go
  where
    go :: Type -> [Name]
    go (AppT t1 t2) = go t1 ++ go t2
    go (SigT t _k)  = go t
#if MIN_VERSION_template_haskell(2,8,0)
                           ++ go _k
#endif
    go (VarT n)     = [n]
    go _            = []

-- | Gets all of the type/kind variable names mentioned somewhere in a Kind.
tyVarNamesOfKind :: Kind -> [Name]
#if MIN_VERSION_template_haskell(2,8,0)
tyVarNamesOfKind = tyVarNamesOfType
#else
tyVarNamesOfKind _ = [] -- There are no kind variables
#endif

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (concatMap tyVarNamesOfKind uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | If a VarT is missing an explicit kind signature, steal it from a TyVarBndr.
stealKindForType :: TyVarBndr -> Type -> Type
stealKindForType tvb t@VarT{} = SigT t (tvbKind tvb)
stealKindForType _   t        = t

-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | A mapping of type variable Names to their map function Names. For example, in a
-- Bifunctor declaration, a TyVarMap might look like (a ~> f, b ~> g), where
-- a and b are the last two type variables of the datatype, and f and g are the two
-- functions which map their respective type variables.
type TyVarMap = Map Name Name

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con
#if MIN_VERSION_template_haskell(2,11,0)
constructorName (GadtC    names _ _)    = head names
constructorName (RecGadtC names _ _)    = head names
#endif

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Extracts the kind from a TyVarBndr.
tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _)   = starK
tvbKind (KindedTV _ k) = k

-- | Convert a TyVarBndr to a Type.
tvbToType :: TyVarBndr -> Type
tvbToType (PlainTV n)    = VarT n
tvbToType (KindedTV n k) = SigT (VarT n) k

-- | Applies a typeclass constraint to a type.
applyClass :: Name -> Name -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
applyClass con t = AppT (ConT con) (VarT t)
#else
applyClass con t = ClassP con [VarT t]
#endif

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

-- | Is the given type a type family constructor (and not a data family constructor)?
isTyFamily :: Type -> Q Bool
isTyFamily (ConT n) = do
    info <- reify n
    return $ case info of
#if MIN_VERSION_template_haskell(2,11,0)
         FamilyI OpenTypeFamilyD{} _       -> True
#elif MIN_VERSION_template_haskell(2,7,0)
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#else
         TyConI  (FamilyD TypeFam _ _ _)   -> True
#endif
#if MIN_VERSION_template_haskell(2,9,0)
         FamilyI ClosedTypeFamilyD{} _     -> True
#endif
         _ -> False
isTyFamily _ = return False

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
    go (SigT t _k)  names = go t names
#if MIN_VERSION_template_haskell(2,8,0)
                              || go _k names
#endif
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsName = mentionsName
#else
predMentionsName (ClassP n tys) names = n `elem` names || any (`mentionsName` names) tys
predMentionsName (EqualP t1 t2) names = mentionsName t1 names || mentionsName t2 names
#endif

-- | Construct a type via curried application.
applyTy :: Type -> [Type] -> Type
applyTy = foldl' AppT

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
unapplyTy :: Type -> [Type]
unapplyTy = reverse . go
  where
    go :: Type -> [Type]
    go (AppT t1 t2)    = t2:go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = [t]

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
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = snd . uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1:uncurryKind k2
uncurryKind k              = [k]
#endif

-------------------------------------------------------------------------------
-- Manually quoted names
-------------------------------------------------------------------------------

-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling the bifunctors library.
-- This allows the library to be used in stage1 cross-compilers.

bifunctorsPackageKey :: String
#ifdef CURRENT_PACKAGE_KEY
bifunctorsPackageKey = CURRENT_PACKAGE_KEY
#else
bifunctorsPackageKey = "bifunctors-" ++ showVersion version
#endif

mkBifunctorsName_tc :: String -> String -> Name
mkBifunctorsName_tc = mkNameG_tc bifunctorsPackageKey

mkBifunctorsName_v :: String -> String -> Name
mkBifunctorsName_v = mkNameG_v bifunctorsPackageKey

bifoldableTypeName :: Name
bifoldableTypeName = mkBifunctorsName_tc "Data.Bifoldable" "Bifoldable"

bitraversableTypeName :: Name
bitraversableTypeName = mkBifunctorsName_tc "Data.Bitraversable" "Bitraversable"

bifoldrValName :: Name
bifoldrValName = mkBifunctorsName_v "Data.Bifoldable" "bifoldr"

bifoldMapValName :: Name
bifoldMapValName = mkBifunctorsName_v "Data.Bifoldable" "bifoldMap"

bitraverseValName :: Name
bitraverseValName = mkBifunctorsName_v "Data.Bitraversable" "bitraverse"

bimapConstValName :: Name
bimapConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bimapConst"

bifoldrConstValName :: Name
bifoldrConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bifoldrConst"

bifoldMapConstValName :: Name
bifoldMapConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bifoldMapConst"

bitraverseConstValName :: Name
bitraverseConstValName = mkBifunctorsName_v "Data.Bifunctor.TH.Internal" "bitraverseConst"

dualDataName :: Name
dualDataName = mkNameG_d "base" "Data.Monoid" "Dual"

endoDataName :: Name
endoDataName = mkNameG_d "base" "Data.Monoid" "Endo"

wrapMonadDataName :: Name
wrapMonadDataName = mkNameG_d "base" "Control.Applicative" "WrapMonad"

functorTypeName :: Name
functorTypeName = mkNameG_tc "base" "GHC.Base" "Functor"

foldableTypeName :: Name
foldableTypeName = mkNameG_tc "base" "Data.Foldable" "Foldable"

traversableTypeName :: Name
traversableTypeName = mkNameG_tc "base" "Data.Traversable" "Traversable"

appEndoValName :: Name
appEndoValName = mkNameG_v "base" "Data.Monoid" "appEndo"

composeValName :: Name
composeValName = mkNameG_v "base" "GHC.Base" "."

idValName :: Name
idValName = mkNameG_v "base" "GHC.Base" "id"

errorValName :: Name
errorValName = mkNameG_v "base" "GHC.Err" "error"

flipValName :: Name
flipValName = mkNameG_v "base" "GHC.Base" "flip"

fmapValName :: Name
fmapValName = mkNameG_v "base" "GHC.Base" "fmap"

foldrValName :: Name
foldrValName = mkNameG_v "base" "Data.Foldable" "foldr"

foldMapValName :: Name
foldMapValName = mkNameG_v "base" "Data.Foldable" "foldMap"

getDualValName :: Name
getDualValName = mkNameG_v "base" "Data.Monoid" "getDual"

traverseValName :: Name
traverseValName = mkNameG_v "base" "Data.Traversable" "traverse"

unwrapMonadValName :: Name
unwrapMonadValName = mkNameG_v "base" "Control.Applicative" "unwrapMonad"

#if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,9,0))
starKindName :: Name
starKindName = mkNameG_tc "ghc-prim" "GHC.Prim" "*"
#endif

#if MIN_VERSION_base(4,8,0)
bifunctorTypeName :: Name
bifunctorTypeName = mkNameG_tc "base" "Data.Bifunctor" "Bifunctor"

bimapValName :: Name
bimapValName = mkNameG_v "base" "Data.Bifunctor" "bimap"

pureValName :: Name
pureValName = mkNameG_v "base" "GHC.Base" "pure"

apValName :: Name
apValName = mkNameG_v "base" "GHC.Base" "<*>"

mappendValName :: Name
mappendValName = mkNameG_v "base" "GHC.Base" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "GHC.Base" "mempty"
#else
bifunctorTypeName :: Name
bifunctorTypeName = mkBifunctorsName_tc "Data.Bifunctor" "Bifunctor"

bimapValName :: Name
bimapValName = mkBifunctorsName_v "Data.Bifunctor" "bimap"

pureValName :: Name
pureValName = mkNameG_v "base" "Control.Applicative" "pure"

apValName :: Name
apValName = mkNameG_v "base" "Control.Applicative" "<*>"

mappendValName :: Name
mappendValName = mkNameG_v "base" "Data.Monoid" "mappend"

memptyValName :: Name
memptyValName = mkNameG_v "base" "Data.Monoid" "mempty"
#endif
