{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett, (C) 2015-2016 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions to mechanically derive 'Bifunctor', 'Bifoldable',
-- or 'Bitraversable' instances, or to splice their functions directly into
-- source code. You need to enable the @TemplateHaskell@ language extension
-- in order to use this module.
----------------------------------------------------------------------------

module Data.Bifunctor.TH (
    -- * @derive@- functions
    -- $derive
    -- * @make@- functions
    -- $make
    -- * 'Bifunctor'
    deriveBifunctor
  , deriveBifunctorOptions
  , makeBimap
  , makeBimapOptions
    -- * 'Bifoldable'
  , deriveBifoldable
  , deriveBifoldableOptions
  , makeBifold
  , makeBifoldOptions
  , makeBifoldMap
  , makeBifoldMapOptions
  , makeBifoldr
  , makeBifoldrOptions
  , makeBifoldl
  , makeBifoldlOptions
    -- * 'Bitraversable'
  , deriveBitraversable
  , deriveBitraversableOptions
  , makeBitraverse
  , makeBitraverseOptions
  , makeBisequenceA
  , makeBisequenceAOptions
  , makeBimapM
  , makeBimapMOptions
  , makeBisequence
  , makeBisequenceOptions
    -- * 'Options'
  , Options(..)
  , defaultOptions
  ) where

import           Control.Monad (guard, unless, when, zipWithM)

import           Data.Bifunctor.TH.Internal
import           Data.Either (rights)
import           Data.List
import qualified Data.Map as Map (fromList, keys, lookup, size)
import           Data.Maybe

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- User-facing API
-------------------------------------------------------------------------------

-- | Options that further configure how the functions in "Data.Bifunctor.TH"
-- should behave.
newtype Options = Options
  { emptyCaseBehavior :: Bool
    -- ^ If 'True', derived instances for empty data types (i.e., ones with
    --   no data constructors) will use the @EmptyCase@ language extension.
    --   If 'False', derived instances will simply use 'seq' instead.
    --   (This has no effect on GHCs before 7.8, since @EmptyCase@ is only
    --   available in 7.8 or later.)
  } deriving (Eq, Ord, Read, Show)

-- | Conservative 'Options' that doesn't attempt to use @EmptyCase@ (to
-- prevent users from having to enable that extension at use sites.)
defaultOptions :: Options
defaultOptions = Options { emptyCaseBehavior = False }

{- $derive

'deriveBifunctor', 'deriveBifoldable', and 'deriveBitraversable' automatically
generate their respective class instances for a given data type, newtype, or data
family instance that has at least two type variable. Examples:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
import Data.Bifunctor.TH

data Pair a b = Pair a b
$('deriveBifunctor' ''Pair) -- instance Bifunctor Pair where ...

data WrapLeftPair f g a b = WrapLeftPair (f a) (g a b)
$('deriveBifoldable' ''WrapLeftPair)
-- instance (Foldable f, Bifoldable g) => Bifoldable (WrapLeftPair f g) where ...
@

If you are using @template-haskell-2.7.0.0@ or later (i.e., GHC 7.4 or later),
the @derive@ functions can be used data family instances (which requires the
@-XTypeFamilies@ extension). To do so, pass the name of a data or newtype instance
constructor (NOT a data family name!) to a @derive@ function.  Note that the
generated code may require the @-XFlexibleInstances@ extension. Example:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;
import Data.Bifunctor.TH

class AssocClass a b c where
    data AssocData a b c
instance AssocClass Int b c where
    data AssocData Int b c = AssocDataInt1 Int | AssocDataInt2 b c
$('deriveBitraversable' 'AssocDataInt1) -- instance Bitraversable (AssocData Int) where ...
-- Alternatively, one could use $(deriveBitraversable 'AssocDataInt2)
@

Note that there are some limitations:

* The 'Name' argument to a @derive@ function must not be a type synonym.

* With a @derive@ function, the last two type variables must both be of kind @*@.
  Other type variables of kind @* -> *@ are assumed to require a 'Functor',
  'Foldable', or 'Traversable' constraint (depending on which @derive@ function is
  used), and other type variables of kind @* -> * -> *@ are assumed to require an
  'Bifunctor', 'Bifoldable', or 'Bitraversable' constraint. If your data type
  doesn't meet these assumptions, use a @make@ function.

* If using the @-XDatatypeContexts@, @-XExistentialQuantification@, or @-XGADTs@
  extensions, a constraint cannot mention either of the last two type variables. For
  example, @data Illegal2 a b where I2 :: Ord a => a -> b -> Illegal2 a b@ cannot
  have a derived 'Bifunctor' instance.

* If either of the last two type variables is used within a constructor argument's
  type, it must only be used in the last two type arguments. For example,
  @data Legal a b = Legal (Int, Int, a, b)@ can have a derived 'Bifunctor' instance,
  but @data Illegal a b = Illegal (a, b, a, b)@ cannot.

* Data family instances must be able to eta-reduce the last two type variables. In other
  words, if you have a instance of the form:

  @
  data family Family a1 ... an t1 t2
  data instance Family e1 ... e2 v1 v2 = ...
  @

  Then the following conditions must hold:

  1. @v1@ and @v2@ must be distinct type variables.
  2. Neither @v1@ not @v2@ must be mentioned in any of @e1@, ..., @e2@.

-}

{- $make

There may be scenarios in which you want to, say, 'bimap' over an arbitrary data type
or data family instance without having to make the type an instance of 'Bifunctor'. For
these cases, this module provides several functions (all prefixed with @make@-) that
splice the appropriate lambda expression into your source code.

This is particularly useful for creating instances for sophisticated data types. For
example, 'deriveBifunctor' cannot infer the correct type context for
@newtype HigherKinded f a b c = HigherKinded (f a b c)@, since @f@ is of kind
@* -> * -> * -> *@. However, it is still possible to create a 'Bifunctor' instance for
@HigherKinded@ without too much trouble using 'makeBimap':

@
&#123;-&#35; LANGUAGE FlexibleContexts, TemplateHaskell &#35;-&#125;
import Data.Bifunctor
import Data.Bifunctor.TH

newtype HigherKinded f a b c = HigherKinded (f a b c)

instance Bifunctor (f a) => Bifunctor (HigherKinded f a) where
    bimap = $(makeBimap ''HigherKinded)
@

-}

-- | Generates a 'Bifunctor' instance declaration for the given data type or data
-- family instance.
deriveBifunctor :: Name -> Q [Dec]
deriveBifunctor = deriveBifunctorOptions defaultOptions

-- | Like 'deriveBifunctor', but takes an 'Options' argument.
deriveBifunctorOptions :: Options -> Name -> Q [Dec]
deriveBifunctorOptions = deriveBiClass Bifunctor

-- | Generates a lambda expression which behaves like 'bimap' (without requiring a
-- 'Bifunctor' instance).
makeBimap :: Name -> Q Exp
makeBimap = makeBimapOptions defaultOptions

-- | Like 'makeBimap', but takes an 'Options' argument.
makeBimapOptions :: Options -> Name -> Q Exp
makeBimapOptions = makeBiFun Bimap

-- | Generates a 'Bifoldable' instance declaration for the given data type or data
-- family instance.
deriveBifoldable :: Name -> Q [Dec]
deriveBifoldable = deriveBifoldableOptions defaultOptions

-- | Like 'deriveBifoldable', but takes an 'Options' argument.
deriveBifoldableOptions :: Options -> Name -> Q [Dec]
deriveBifoldableOptions = deriveBiClass Bifoldable

--- | Generates a lambda expression which behaves like 'bifold' (without requiring a
-- 'Bifoldable' instance).
makeBifold :: Name -> Q Exp
makeBifold = makeBifoldOptions defaultOptions

-- | Like 'makeBifold', but takes an 'Options' argument.
makeBifoldOptions :: Options -> Name -> Q Exp
makeBifoldOptions opts name = appsE [ makeBifoldMapOptions opts name
                                    , varE idValName
                                    , varE idValName
                                    ]

-- | Generates a lambda expression which behaves like 'bifoldMap' (without requiring
-- a 'Bifoldable' instance).
makeBifoldMap :: Name -> Q Exp
makeBifoldMap = makeBifoldMapOptions defaultOptions

-- | Like 'makeBifoldMap', but takes an 'Options' argument.
makeBifoldMapOptions :: Options -> Name -> Q Exp
makeBifoldMapOptions = makeBiFun BifoldMap

-- | Generates a lambda expression which behaves like 'bifoldr' (without requiring a
-- 'Bifoldable' instance).
makeBifoldr :: Name -> Q Exp
makeBifoldr = makeBifoldrOptions defaultOptions

-- | Like 'makeBifoldr', but takes an 'Options' argument.
makeBifoldrOptions :: Options -> Name -> Q Exp
makeBifoldrOptions = makeBiFun Bifoldr

-- | Generates a lambda expression which behaves like 'bifoldl' (without requiring a
-- 'Bifoldable' instance).
makeBifoldl :: Name -> Q Exp
makeBifoldl = makeBifoldlOptions defaultOptions

-- | Like 'makeBifoldl', but takes an 'Options' argument.
makeBifoldlOptions :: Options -> Name -> Q Exp
makeBifoldlOptions opts name = do
  f <- newName "f"
  g <- newName "g"
  z <- newName "z"
  t <- newName "t"
  lamE [varP f, varP g, varP z, varP t] $
    appsE [ varE appEndoValName
          , appsE [ varE getDualValName
                  , appsE [ makeBifoldMapOptions opts name
                          , foldFun f
                          , foldFun g
                          , varE t]
                  ]
          , varE z
          ]
  where
    foldFun :: Name -> Q Exp
    foldFun n = infixApp (conE dualDataName)
                         (varE composeValName)
                         (infixApp (conE endoDataName)
                                   (varE composeValName)
                                   (varE flipValName `appE` varE n)
                         )

-- | Generates a 'Bitraversable' instance declaration for the given data type or data
-- family instance.
deriveBitraversable :: Name -> Q [Dec]
deriveBitraversable = deriveBitraversableOptions defaultOptions

-- | Like 'deriveBitraversable', but takes an 'Options' argument.
deriveBitraversableOptions :: Options -> Name -> Q [Dec]
deriveBitraversableOptions = deriveBiClass Bitraversable

-- | Generates a lambda expression which behaves like 'bitraverse' (without
-- requiring a 'Bitraversable' instance).
makeBitraverse :: Name -> Q Exp
makeBitraverse = makeBitraverseOptions defaultOptions

-- | Like 'makeBitraverse', but takes an 'Options' argument.
makeBitraverseOptions :: Options -> Name -> Q Exp
makeBitraverseOptions = makeBiFun Bitraverse

-- | Generates a lambda expression which behaves like 'bisequenceA' (without
-- requiring a 'Bitraversable' instance).
makeBisequenceA :: Name -> Q Exp
makeBisequenceA = makeBisequenceAOptions defaultOptions

-- | Like 'makeBitraverseA', but takes an 'Options' argument.
makeBisequenceAOptions :: Options -> Name -> Q Exp
makeBisequenceAOptions opts name = appsE [ makeBitraverseOptions opts name
                                         , varE idValName
                                         , varE idValName
                                         ]

-- | Generates a lambda expression which behaves like 'bimapM' (without
-- requiring a 'Bitraversable' instance).
makeBimapM :: Name -> Q Exp
makeBimapM = makeBimapMOptions defaultOptions

-- | Like 'makeBimapM', but takes an 'Options' argument.
makeBimapMOptions :: Options -> Name -> Q Exp
makeBimapMOptions opts name = do
  f <- newName "f"
  g <- newName "g"
  lamE [varP f, varP g] . infixApp (varE unwrapMonadValName) (varE composeValName) $
                          appsE [ makeBitraverseOptions opts name
                                , wrapMonadExp f
                                , wrapMonadExp g
                                ]
  where
    wrapMonadExp :: Name -> Q Exp
    wrapMonadExp n = infixApp (conE wrapMonadDataName) (varE composeValName) (varE n)

-- | Generates a lambda expression which behaves like 'bisequence' (without
-- requiring a 'Bitraversable' instance).
makeBisequence :: Name -> Q Exp
makeBisequence = makeBisequenceOptions defaultOptions

-- | Like 'makeBisequence', but takes an 'Options' argument.
makeBisequenceOptions :: Options -> Name -> Q Exp
makeBisequenceOptions opts name = appsE [ makeBimapMOptions opts name
                                        , varE idValName
                                        , varE idValName
                                        ]

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a class instance declaration (depending on the BiClass argument's value).
deriveBiClass :: BiClass -> Options -> Name -> Q [Dec]
deriveBiClass biClass opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTys
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
          <- buildTypeInstance biClass parentName ctxt instTys variant
      (:[]) `fmap` instanceD (return instanceCxt)
                             (return instanceType)
                             (biFunDecs biClass opts parentName instTys cons)

-- | Generates a declaration defining the primary function(s) corresponding to a
-- particular class (bimap for Bifunctor, bifoldr and bifoldMap for Bifoldable, and
-- bitraverse for Bitraversable).
--
-- For why both bifoldr and bifoldMap are derived for Bifoldable, see Trac #7436.
biFunDecs :: BiClass -> Options -> Name -> [Type] -> [ConstructorInfo] -> [Q Dec]
biFunDecs biClass opts parentName instTys cons =
  map makeFunD $ biClassToFuns biClass
  where
    makeFunD :: BiFun -> Q Dec
    makeFunD biFun =
      funD (biFunName biFun)
           [ clause []
                    (normalB $ makeBiFunForCons biFun opts parentName instTys cons)
                    []
           ]

-- | Generates a lambda expression which behaves like the BiFun argument.
makeBiFun :: BiFun -> Options -> Name -> Q Exp
makeBiFun biFun opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTys
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } ->
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype can actually have bimap/bifoldr/bitraverse/etc.
      -- implemented for it, and produces errors if it can't.
      buildTypeInstance (biFunToClass biFun) parentName ctxt instTys variant
        >> makeBiFunForCons biFun opts parentName instTys cons

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeBiFunForCons :: BiFun -> Options -> Name -> [Type] -> [ConstructorInfo] -> Q Exp
makeBiFunForCons biFun opts _parentName instTys cons = do
  argNames <- mapM newName $ catMaybes [ Just "f"
                                       , Just "g"
                                       , guard (biFun == Bifoldr) >> Just "z"
                                       , Just "value"
                                       ]
  let ([map1, map2], others) = splitAt 2 argNames
      z          = head others -- If we're deriving bifoldr, this will be well defined
                               -- and useful. Otherwise, it'll be ignored.
      value      = last others
      lastTyVars = map varTToName $ drop (length instTys - 2) instTys
      tvMap      = Map.fromList $ zip lastTyVars [map1, map2]
  lamE (map varP argNames)
      . appsE
      $ [ varE $ biFunConstName biFun
        , makeFun z value tvMap
        ] ++ map varE argNames
  where
    makeFun :: Name -> Name -> TyVarMap -> Q Exp
    makeFun z value tvMap = do
#if MIN_VERSION_template_haskell(2,9,0)
      roles <- reifyRoles _parentName
#endif
      case () of
        _

#if MIN_VERSION_template_haskell(2,9,0)
          | Just (rs, PhantomR) <- unsnoc roles
          , Just (_,  PhantomR) <- unsnoc rs
         -> biFunPhantom z value
#endif

          | null cons && emptyCaseBehavior opts && ghc7'8OrLater
         -> biFunEmptyCase biFun z value

          | null cons
         -> biFunNoCons biFun z value

          | otherwise
         -> caseE (varE value)
                  (map (makeBiFunForCon biFun z tvMap) cons)

    ghc7'8OrLater :: Bool
#if __GLASGOW_HASKELL__ >= 708
    ghc7'8OrLater = True
#else
    ghc7'8OrLater = False
#endif

#if MIN_VERSION_template_haskell(2,9,0)
    biFunPhantom :: Name -> Name -> Q Exp
    biFunPhantom z value =
        biFunTrivial coerce
                     (varE pureValName `appE` coerce)
                     biFun z
      where
        coerce :: Q Exp
        coerce = varE coerceValName `appE` varE value
#endif

-- | Generates a lambda expression for a single constructor.
makeBiFunForCon :: BiFun -> Name -> TyVarMap -> ConstructorInfo -> Q Match
makeBiFunForCon biFun z tvMap
  (ConstructorInfo { constructorName    = conName
                   , constructorContext = ctxt
                   , constructorFields  = ts }) = do
    ts'      <- mapM resolveTypeSynonyms ts
    argNames <- newNameList "_arg" $ length ts'
    if (any (`predMentionsName` Map.keys tvMap) ctxt
          || Map.size tvMap < 2)
          && not (allowExQuant (biFunToClass biFun))
       then existentialContextError conName
       else makeBiFunForArgs biFun z tvMap conName ts' argNames

-- | Generates a lambda expression for a single constructor's arguments.
makeBiFunForArgs :: BiFun
                 -> Name
                 -> TyVarMap
                 -> Name
                 -> [Type]
                 -> [Name]
                 -> Q Match
makeBiFunForArgs biFun z tvMap conName tys args =
  match (conP conName $ map varP args)
        (normalB $ biFunCombine biFun conName z args mappedArgs)
        []
  where
    mappedArgs :: Q [Either Exp Exp]
    mappedArgs = zipWithM (makeBiFunForArg biFun tvMap conName) tys args

-- | Generates a lambda expression for a single argument of a constructor.
--  The returned value is 'Right' if its type mentions one of the last two type
-- parameters. Otherwise, it is 'Left'.
makeBiFunForArg :: BiFun
                -> TyVarMap
                -> Name
                -> Type
                -> Name
                -> Q (Either Exp Exp)
makeBiFunForArg biFun tvMap conName ty tyExpName =
  makeBiFunForType biFun tvMap conName True ty `appEitherE` varE tyExpName

-- | Generates a lambda expression for a specific type. The returned value is
-- 'Right' if its type mentions one of the last two type parameters. Otherwise,
-- it is 'Left'.
makeBiFunForType :: BiFun
                 -> TyVarMap
                 -> Name
                 -> Bool
                 -> Type
                 -> Q (Either Exp Exp)
makeBiFunForType biFun tvMap conName covariant (VarT tyName) =
  case Map.lookup tyName tvMap of
    Just mapName -> fmap Right . varE $
                        if covariant
                           then mapName
                           else contravarianceError conName
    Nothing -> fmap Left $ biFunTriv biFun
makeBiFunForType biFun tvMap conName covariant (SigT ty _) =
  makeBiFunForType biFun tvMap conName covariant ty
makeBiFunForType biFun tvMap conName covariant (ForallT _ _ ty) =
  makeBiFunForType biFun tvMap conName covariant ty
makeBiFunForType biFun tvMap conName covariant ty =
  let tyCon  :: Type
      tyArgs :: [Type]
      tyCon:tyArgs = unapplyTy ty

      numLastArgs :: Int
      numLastArgs = min 2 $ length tyArgs

      lhsArgs, rhsArgs :: [Type]
      (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

      tyVarNames :: [Name]
      tyVarNames = Map.keys tvMap

      mentionsTyArgs :: Bool
      mentionsTyArgs = any (`mentionsName` tyVarNames) tyArgs

      makeBiFunTuple :: ([Q Pat] -> Q Pat) -> (Int -> Name) -> Int
                     -> Q (Either Exp Exp)
      makeBiFunTuple mkTupP mkTupleDataName n = do
        args <- mapM newName $ catMaybes [ Just "x"
                                         , guard (biFun == Bifoldr) >> Just "z"
                                         ]
        xs <- newNameList "_tup" n

        let x = head args
            z = last args
        fmap Right $ lamE (map varP args) $ caseE (varE x)
             [ match (mkTupP $ map varP xs)
                     (normalB $ biFunCombine biFun
                                             (mkTupleDataName n)
                                             z
                                             xs
                                             (zipWithM makeBiFunTupleField tyArgs xs)
                     )
                     []
             ]

      makeBiFunTupleField :: Type -> Name -> Q (Either Exp Exp)
      makeBiFunTupleField fieldTy fieldName =
        makeBiFunForType biFun tvMap conName covariant fieldTy
          `appEitherE` varE fieldName

   in case tyCon of
     ArrowT
       | not (allowFunTys (biFunToClass biFun)) -> noFunctionsError conName
       | mentionsTyArgs, [argTy, resTy] <- tyArgs ->
         do x <- newName "x"
            b <- newName "b"
            fmap Right . lamE [varP x, varP b] $
              covBiFun covariant resTy `appE` (varE x `appE`
                (covBiFun (not covariant) argTy `appE` varE b))
         where
           covBiFun :: Bool -> Type -> Q Exp
           covBiFun cov = fmap fromEither . makeBiFunForType biFun tvMap conName cov
#if MIN_VERSION_template_haskell(2,6,0)
     UnboxedTupleT n
       | n > 0 && mentionsTyArgs -> makeBiFunTuple unboxedTupP unboxedTupleDataName n
#endif
     TupleT n
       | n > 0 && mentionsTyArgs -> makeBiFunTuple tupP tupleDataName n
     _ -> do
         itf <- isTyFamily tyCon
         if any (`mentionsName` tyVarNames) lhsArgs || (itf && mentionsTyArgs)
           then outOfPlaceTyVarError conName
           else if any (`mentionsName` tyVarNames) rhsArgs
                  then fmap Right . biFunApp biFun . appsE $
                         ( varE (fromJust $ biFunArity biFun numLastArgs)
                         : map (fmap fromEither . makeBiFunForType biFun tvMap conName covariant)
                                rhsArgs
                         )
                  else fmap Left $ biFunTriv biFun

-------------------------------------------------------------------------------
-- Template Haskell reifying and AST manipulation
-------------------------------------------------------------------------------

-- For the given Types, generate an instance context and head. Coming up with
-- the instance type isn't as simple as dropping the last types, as you need to
-- be wary of kinds being instantiated with *.
-- See Note [Type inference in derived instances]
buildTypeInstance :: BiClass
                  -- ^ Bifunctor, Bifoldable, or Bitraversable
                  -> Name
                  -- ^ The type constructor or data family name
                  -> Cxt
                  -- ^ The datatype context
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> DatatypeVariant
                  -- ^ Are we dealing with a data family instance or not
                  -> Q (Cxt, Type)
buildTypeInstance biClass tyConName dataCxt instTysOrig variant = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms instTysOrig

    let remainingLength :: Int
        remainingLength = length instTysOrig - 2

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || any (== NotKindStar) droppedStarKindStati) $
      derivingKindError biClass tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = freeVariables droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError biClass tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint biClass) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (union droppedKindVarNames kvNames'))
            $ take remainingLength instTysOrig

        isDataFamily :: Bool
        isDataFamily = case variant of
                         Datatype        -> False
                         Newtype         -> False
                         DataInstance    -> True
                         NewtypeInstance -> True

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ biClassName biClass)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
deriveConstraint :: BiClass -> Type -> (Maybe Pred, [Name])
deriveConstraint biClass t
  | not (isTyVar t) = (Nothing, [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns -> ((`applyClass` tName) `fmap` biClassConstraint biClass 1, ns)
      _ -> case hasKindVarChain 2 t of
                Just ns -> ((`applyClass` tName) `fmap` biClassConstraint biClass 2, ns)
                _       -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

{-
Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since our type inferencer is pretty
unsophisticated - see Note [Type inference in derived instances]), then GHC will
flat-out reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the make- functions.

Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:

1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had

     data Data (a :: k) (b :: k) (c :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criteria:

   (i)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
        variables), then generate a Functor n constraint, and if k1/k2 are kind
        variables, then substitute k1/k2 with * elsewhere in the types. We must
        consider the case where they are kind variables because you might have a
        scenario like this:

          newtype Compose (f :: k3 -> *) (g :: k1 -> k2 -> k3) (a :: k1) (b :: k2)
            = Compose (f (g a b))

        Which would have a derived Bifunctor instance of:

          instance (Functor f, Bifunctor g) => Bifunctor (Compose f g) where ...
   (ii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
        * or kind variables), then generate a Bifunctor n constraint and perform
        kind substitution as in the other case.
-}

{-
Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When deriving Bifoldable, there is a tricky corner case to consider:

  data Both a b where
    BothCon :: x -> x -> Both x x

Which fold functions should be applied to which arguments of BothCon? We have a
choice, since both the function of type (a -> m) and of type (b -> m) can be
applied to either argument. In such a scenario, the second fold function takes
precedence over the first fold function, so the derived Bifoldable instance would be:

  instance Bifoldable Both where
    bifoldMap _ g (BothCon x1 x2) = g x1 <> g x2

This is not an arbitrary choice, as this definition ensures that
bifoldMap id = Foldable.foldMap for a derived Bifoldable instance for Both.
-}

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: BiClass -> Name -> a
derivingKindError biClass tyConName = error
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind * -> * -> *"
  $ ""
  where
    className :: String
    className = nameBase $ biClassName biClass

-- | One of the last two type variables appeard in a contravariant position
-- when deriving Bifoldable or Bitraversable.
contravarianceError :: Name -> a
contravarianceError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must not use the last type variable(s) in a function argument"
  $ ""

-- | A constructor has a function argument in a derived Bifoldable or Bitraversable
-- instance.
noFunctionsError :: Name -> a
noFunctionsError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must not contain function types"
  $ ""

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> a
datatypeContextError dataName instanceType = error
  . showString "Can't make a derived instance of ‘"
  . showString (pprint instanceType)
  . showString "‘:\n\tData type ‘"
  . showString (nameBase dataName)
  . showString "‘ must not have a class context involving the last type argument(s)"
  $ ""

-- | The data type has an existential constraint which mentions one of the
-- eta-reduced type variables.
existentialContextError :: Name -> a
existentialContextError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must be truly polymorphic in the last argument(s) of the data type"
  $ ""

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
outOfPlaceTyVarError :: Name -> a
outOfPlaceTyVarError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must only use its last two type variable(s) within"
  . showString " the last two argument(s) of a data type"
  $ ""

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> a
etaReductionError instanceType = error $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of which class is being derived.
data BiClass = Bifunctor | Bifoldable | Bitraversable

-- | A representation of which function is being generated.
data BiFun = Bimap | Bifoldr | BifoldMap | Bitraverse
  deriving Eq

biFunConstName :: BiFun -> Name
biFunConstName Bimap      = bimapConstValName
biFunConstName Bifoldr    = bifoldrConstValName
biFunConstName BifoldMap  = bifoldMapConstValName
biFunConstName Bitraverse = bitraverseConstValName

biClassName :: BiClass -> Name
biClassName Bifunctor     = bifunctorTypeName
biClassName Bifoldable    = bifoldableTypeName
biClassName Bitraversable = bitraversableTypeName

biFunName :: BiFun -> Name
biFunName Bimap      = bimapValName
biFunName Bifoldr    = bifoldrValName
biFunName BifoldMap  = bifoldMapValName
biFunName Bitraverse = bitraverseValName

biClassToFuns :: BiClass -> [BiFun]
biClassToFuns Bifunctor     = [Bimap]
biClassToFuns Bifoldable    = [Bifoldr, BifoldMap]
biClassToFuns Bitraversable = [Bitraverse]

biFunToClass :: BiFun -> BiClass
biFunToClass Bimap      = Bifunctor
biFunToClass Bifoldr    = Bifoldable
biFunToClass BifoldMap  = Bifoldable
biFunToClass Bitraverse = Bitraversable

biClassConstraint :: BiClass -> Int -> Maybe Name
biClassConstraint Bifunctor     1 = Just functorTypeName
biClassConstraint Bifoldable    1 = Just foldableTypeName
biClassConstraint Bitraversable 1 = Just traversableTypeName
biClassConstraint biClass       2 = Just $ biClassName biClass
biClassConstraint _             _ = Nothing

biFunArity :: BiFun -> Int -> Maybe Name
biFunArity Bimap      1 = Just fmapValName
biFunArity Bifoldr    1 = Just foldrValName
biFunArity BifoldMap  1 = Just foldMapValName
biFunArity Bitraverse 1 = Just traverseValName
biFunArity biFun      2 = Just $ biFunName biFun
biFunArity _          _ = Nothing

allowFunTys :: BiClass -> Bool
allowFunTys Bifunctor = True
allowFunTys _         = False

allowExQuant :: BiClass -> Bool
allowExQuant Bifoldable = True
allowExQuant _          = False

-- See Trac #7436 for why explicit lambdas are used
biFunTriv :: BiFun -> Q Exp
biFunTriv Bimap = do
  x <- newName "x"
  lamE [varP x] $ varE x
-- The biFunTriv definitions for bifoldr, bifoldMap, and bitraverse might seem
-- useless, but they do serve a purpose.
-- See Note [biFunTriv for Bifoldable and Bitraversable]
biFunTriv Bifoldr = do
  z <- newName "z"
  lamE [wildP, varP z] $ varE z
biFunTriv BifoldMap = lamE [wildP] $ varE memptyValName
biFunTriv Bitraverse = varE pureValName

biFunApp :: BiFun -> Q Exp -> Q Exp
biFunApp Bifoldr e = do
  x <- newName "x"
  z <- newName "z"
  lamE [varP x, varP z] $ appsE [e, varE z, varE x]
biFunApp _ e = e

biFunCombine :: BiFun
             -> Name
             -> Name
             -> [Name]
             -> Q [Either Exp Exp]
             -> Q Exp
biFunCombine Bimap      = bimapCombine
biFunCombine Bifoldr    = bifoldrCombine
biFunCombine BifoldMap  = bifoldMapCombine
biFunCombine Bitraverse = bitraverseCombine

bimapCombine :: Name
             -> Name
             -> [Name]
             -> Q [Either Exp Exp]
             -> Q Exp
bimapCombine conName _ _ = fmap (foldl' AppE (ConE conName) . fmap fromEither)

-- bifoldr, bifoldMap, and bitraverse are handled differently from bimap, since
-- they filter out subexpressions whose types do not mention one of the last two
-- type parameters. See
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor#AlternativestrategyforderivingFoldableandTraversable
-- for further discussion.

bifoldrCombine :: Name
               -> Name
               -> [Name]
               -> Q [Either Exp Exp]
               -> Q Exp
bifoldrCombine _ zName _ = fmap (foldr AppE (VarE zName) . rights)

bifoldMapCombine :: Name
                 -> Name
                 -> [Name]
                 -> Q [Either Exp Exp]
                 -> Q Exp
bifoldMapCombine _ _ _ = fmap (go . rights)
  where
    go :: [Exp] -> Exp
    go [] = VarE memptyValName
    go es = foldr1 (AppE . AppE (VarE mappendValName)) es

bitraverseCombine :: Name
                  -> Name
                  -> [Name]
                  -> Q [Either Exp Exp]
                  -> Q Exp
bitraverseCombine conName _ args essQ = do
    ess <- essQ

    let argTysTyVarInfo :: [Bool]
        argTysTyVarInfo = map isRight ess

        argsWithTyVar, argsWithoutTyVar :: [Name]
        (argsWithTyVar, argsWithoutTyVar) = partitionByList argTysTyVarInfo args

        conExpQ :: Q Exp
        conExpQ
          | null argsWithTyVar
          = appsE (conE conName:map varE argsWithoutTyVar)
          | otherwise = do
              bs <- newNameList "b" $ length args
              let bs'  = filterByList  argTysTyVarInfo bs
                  vars = filterByLists argTysTyVarInfo
                                       (map varE bs) (map varE args)
              lamE (map varP bs') (appsE (conE conName:vars))

    conExp <- conExpQ

    let go :: [Exp] -> Exp
        go []  = VarE pureValName `AppE` conExp
        go [e] = VarE fmapValName `AppE` conExp `AppE` e
        go (e1:e2:es) = foldl' (\se1 se2 -> InfixE (Just se1) (VarE apValName) (Just se2))
          (VarE liftA2ValName `AppE` conExp `AppE` e1 `AppE` e2) es

    return . go . rights $ ess

biFunEmptyCase :: BiFun -> Name -> Name -> Q Exp
biFunEmptyCase biFun z value =
    biFunTrivial emptyCase
                 (varE pureValName `appE` emptyCase)
                 biFun z
  where
    emptyCase :: Q Exp
    emptyCase = caseE (varE value) []

biFunNoCons :: BiFun -> Name -> Name -> Q Exp
biFunNoCons biFun z value =
    biFunTrivial seqAndError
                 (varE pureValName `appE` seqAndError)
                 biFun z
  where
    seqAndError :: Q Exp
    seqAndError = appE (varE seqValName) (varE value) `appE`
                  appE (varE errorValName)
                        (stringE $ "Void " ++ nameBase (biFunName biFun))

biFunTrivial :: Q Exp -> Q Exp -> BiFun -> Name -> Q Exp
biFunTrivial bimapE bitraverseE biFun z = go biFun
  where
    go :: BiFun -> Q Exp
    go Bimap      = bimapE
    go Bifoldr    = varE z
    go BifoldMap  = varE memptyValName
    go Bitraverse = bitraverseE

{-
Note [biFunTriv for Bifoldable and Bitraversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When deriving Bifoldable and Bitraversable, we filter out any subexpressions whose
type does not mention one of the last two type parameters. From this, you might
think that we don't need to implement biFunTriv for bifoldr, bifoldMap, or
bitraverse at all, but in fact we do need to. Imagine the following data type:

    data T a b = MkT a (T Int b)

In a derived Bifoldable T instance, you would generate the following bifoldMap
definition:

    bifoldMap f g (MkT a1 a2) = f a1 <> bifoldMap (\_ -> mempty) g arg2

You need to fill in biFunTriv (\_ -> mempty) as the first argument to the recursive
call to bifoldMap, since that is how the algorithm handles polymorphic recursion.
-}
