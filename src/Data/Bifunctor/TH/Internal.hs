{-# LANGUAGE CPP #-}

{-|
Module:      Data.Bifunctor.TH.Internal
Copyright:   (C) 2008-2015 Edward Kmett, (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Edward Kmett
Portability: Template Haskell

Template Haskell-related utilities.
-}
module Data.Bifunctor.TH.Internal where

import           Data.Function (on)
import           Data.List
import qualified Data.Map as Map (fromList, lookup)
import           Data.Map (Map)
import           Data.Maybe
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
expandSyn (SigT t _)          = expandSyn t   -- Ignore kind synonyms
expandSyn t                   = return t

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
                rhs' = subst subs rhs
             in expandSynApp rhs' ts''
        _ -> return $ foldl' AppT t ts
expandSynApp t ts = do
    t' <- expandSyn t
    return $ foldl' AppT t' ts

type Subst = Map Name Type

mkSubst :: [TyVarBndr] -> [Type] -> Subst
mkSubst vs ts =
   let vs' = map un vs
       un (PlainTV v)    = v
       un (KindedTV v _) = v
   in Map.fromList $ zip vs' ts

subst :: Subst -> Type -> Type
subst subs (ForallT v c t) = ForallT v c $ subst subs t
subst subs t@(VarT n)      = fromMaybe t $ Map.lookup n subs
subst subs (AppT t1 t2)    = AppT (subst subs t1) (subst subs t2)
subst subs (SigT t k)      = SigT (subst subs t) k
subst _ t                  = t

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
-- NameBase
-------------------------------------------------------------------------------

-- | A wrapper around Name which only uses the 'nameBase' (not the entire Name)
-- to compare for equality. For example, if you had two Names a_123 and a_456,
-- they are not equal as Names, but they are equal as NameBases.
--
-- This is useful when inspecting type variables, since a type variable in an
-- instance context may have a distinct Name from a type variable within an
-- actual constructor declaration, but we'd want to treat them as the same
-- if they have the same 'nameBase' (since that's what the programmer uses to
-- begin with).
newtype NameBase = NameBase { getName :: Name }

getNameBase :: NameBase -> String
getNameBase = nameBase . getName

instance Eq NameBase where
    (==) = (==) `on` getNameBase

instance Ord NameBase where
    compare = compare `on` getNameBase

instance Show NameBase where
    showsPrec p = showsPrec p . getNameBase

-- | A NameBase paired with the name of its map function. For example, when deriving
-- Bifunctor, its list of TyVarInfos might look like [(a, 'f), (b, 'g)].
type TyVarInfo = (NameBase, Name)

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Extracts the name of a constructor.
constructorName :: Con -> Name
constructorName (NormalC name      _  ) = name
constructorName (RecC    name      _  ) = name
constructorName (InfixC  _    name _  ) = name
constructorName (ForallC _    _    con) = constructorName con

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Remove any occurrences of a forall-ed type variable from a list of @TyVarInfo@s.
removeForalled :: [TyVarBndr] -> [TyVarInfo] -> [TyVarInfo]
removeForalled tvbs = filter (not . foralled tvbs)
  where
    foralled :: [TyVarBndr] -> TyVarInfo -> Bool
    foralled tvbs' tvi = fst tvi `elem` map (NameBase . tvbName) tvbs'

-- | Extracts the name from a TyVarBndr.
tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name)   = name
tvbName (KindedTV name _) = name

-- | Extracts the kind from a TyVarBndr.
tvbKind :: TyVarBndr -> Kind
tvbKind (PlainTV  _)   = starK
tvbKind (KindedTV _ k) = k

-- | Replace the Name of a TyVarBndr with one from a Type (if the Type has a Name).
replaceTyVarName :: TyVarBndr -> Type -> TyVarBndr
replaceTyVarName tvb            (SigT t _) = replaceTyVarName tvb t
replaceTyVarName (PlainTV  _)   (VarT n)   = PlainTV  n
replaceTyVarName (KindedTV _ k) (VarT n)   = KindedTV n k
replaceTyVarName tvb            _          = tvb

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
    && allDistinct nbs -- Make sure not to pass something of type [Type], since Type
                       -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsNameBase` nbs) remaining)
  where
    nbs :: [NameBase]
    nbs = map varTToNameBase dropped

-- | Extract the Name from a type variable.
varTToName :: Type -> Name
varTToName (VarT n)   = n
varTToName (SigT t _) = varTToName t
varTToName _          = error "Not a type variable!"

-- | Extract the NameBase from a type variable.
varTToNameBase :: Type -> NameBase
varTToNameBase = NameBase . varTToName

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
#if MIN_VERSION_template_haskell(2,7,0)
         FamilyI (FamilyD TypeFam _ _ _) _ -> True
#else
         TyConI  (FamilyD TypeFam _ _ _)   -> True
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

-- | Does the given type mention any of the NameBases in the list?
mentionsNameBase :: Type -> [NameBase] -> Bool
mentionsNameBase = go Set.empty
  where
    go :: Set NameBase -> Type -> [NameBase] -> Bool
    go foralls (ForallT tvbs _ t) nbs =
        go (foralls `Set.union` Set.fromList (map (NameBase . tvbName) tvbs)) t nbs
    go foralls (AppT t1 t2) nbs = go foralls t1 nbs || go foralls t2 nbs
    go foralls (SigT t _)   nbs = go foralls t nbs
    go foralls (VarT n)     nbs = varNb `elem` nbs && not (varNb `Set.member` foralls)
      where
        varNb = NameBase n
    go _       _            _   = False

-- | Does an instance predicate mention any of the NameBases in the list?
predMentionsNameBase :: Pred -> [NameBase] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsNameBase = mentionsNameBase
#else
predMentionsNameBase (ClassP _ tys) nbs = any (`mentionsNameBase` nbs) tys
predMentionsNameBase (EqualP t1 t2) nbs = mentionsNameBase t1 nbs || mentionsNameBase t2 nbs
#endif

-- | The number of arrows that compose the spine of a kind signature
-- (e.g., (* -> *) -> k -> * has two arrows on its spine).
numKindArrows :: Kind -> Int
numKindArrows k = length (uncurryKind k) - 1

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
    go (AppT t1 t2) = t2:go t1
    go (SigT t _)   = go t
    go t            = [t]

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- (Int -> String) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- [Int -> String, Char, ()]
-- @
uncurryTy :: Type -> [Type]
uncurryTy (AppT (AppT ArrowT t1) t2) = t1:uncurryTy t2
uncurryTy (SigT t _)                 = uncurryTy t
uncurryTy t                          = [t]

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> [Kind]
#if MIN_VERSION_template_haskell(2,8,0)
uncurryKind = uncurryTy
#else
uncurryKind (ArrowK k1 k2) = k1:uncurryKind k2
uncurryKind k              = [k]
#endif

wellKinded :: [Kind] -> Bool
wellKinded = all canRealizeKindStar

-- | Of form k1 -> k2 -> ... -> kn, where k is either a single kind variable or *.
canRealizeKindStarChain :: Kind -> Bool
canRealizeKindStarChain = all canRealizeKindStar . uncurryKind

canRealizeKindStar :: Kind -> Bool
canRealizeKindStar k = case uncurryKind k of
    [k'] -> case k' of
#if MIN_VERSION_template_haskell(2,8,0)
                 StarT    -> True
                 (VarT _) -> True -- Kind k can be instantiated with *
#else
                 StarK    -> True
#endif
                 _ -> False
    _ -> False

distinctKindVars :: Kind -> Set Name
#if MIN_VERSION_template_haskell(2,8,0)
distinctKindVars (AppT k1 k2) = distinctKindVars k1 `Set.union` distinctKindVars k2
distinctKindVars (SigT k _)   = distinctKindVars k
distinctKindVars (VarT k)     = Set.singleton k
#endif
distinctKindVars _            = Set.empty

tvbToType :: TyVarBndr -> Type
tvbToType (PlainTV n)    = VarT n
tvbToType (KindedTV n k) = SigT (VarT n) k

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
