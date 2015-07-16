{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{-|
Module:      Data.Bifunctor.TH
Copyright:   (C) 2008-2015 Edward Kmett, (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Edward Kmett
Portability: Template Haskell

Functions to mechanically derive 'Bifunctor', 'Bifoldable', or 'Bitraversable'
instances, or to splice their functions directly into source code. You need to enable
the @TemplateHaskell@ language extension in order to use this module.
-}
module Data.Bifunctor.TH (
      -- * @derive@- functions
      -- $derive
      -- * @make@- functions
      -- $make
      -- * 'Bifunctor'
      deriveBifunctor
    , makeBimap
      -- * 'Bifoldable'
    , deriveBifoldable
    , makeBifold
    , makeBifoldMap
    , makeBifoldr
    , makeBifoldl
      -- * 'Bitraversable'
    , deriveBitraversable
    , makeBitraverse
    , makeBisequenceA
    , makeBimapM
    , makeBisequence
    ) where

import           Control.Monad (guard)

import           Data.Bifunctor.TH.Internal
import           Data.List
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 710 && MIN_VERSION_template_haskell(2,8,0)
import qualified Data.Set as Set
#endif

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- User-facing API
-------------------------------------------------------------------------------

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

* In GHC 7.8, a bug exists that can cause problems when a data family declaration and
  one of its data instances use different type variables, e.g.,

  @
  data family Foo a b c
  data instance Foo Int y z = Foo Int y z
  $(deriveBifunctor 'Foo)
  @

  To avoid this issue, it is recommened that you use the same type variables in the
  same positions in which they appeared in the data family declaration:

  @
  data family Foo a b c
  data instance Foo Int b c = Foo Int b c
  $(deriveBifunctor 'Foo)
  @

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
deriveBifunctor = deriveBiClass Bifunctor

-- | Generates a lambda expression which behaves like 'bimap' (without requiring a
-- 'Bifunctor' instance).
makeBimap :: Name -> Q Exp
makeBimap = makeBiFun Bimap

-- | Generates a 'Bifoldable' instance declaration for the given data type or data
-- family instance.
deriveBifoldable :: Name -> Q [Dec]
deriveBifoldable = deriveBiClass Bifoldable

-- | Generates a lambda expression which behaves like 'bifold' (without requiring a
-- 'Bifoldable' instance).
makeBifold :: Name -> Q Exp
makeBifold name = appsE [ makeBifoldMap name
                        , varE idValName
                        , varE idValName
                        ]

-- | Generates a lambda expression which behaves like 'bifoldMap' (without requiring a
-- 'Bifoldable' instance).
makeBifoldMap :: Name -> Q Exp
makeBifoldMap = makeBiFun BifoldMap

-- | Generates a lambda expression which behaves like 'bifoldr' (without requiring a
-- 'Bifoldable' instance).
makeBifoldr :: Name -> Q Exp
makeBifoldr = makeBiFun Bifoldr

-- | Generates a lambda expression which behaves like 'bifoldl' (without requiring a
-- 'Bifoldable' instance).
makeBifoldl :: Name -> Q Exp
makeBifoldl name = do
    f <- newName "f"
    g <- newName "g"
    z <- newName "z"
    t <- newName "t"
    lamE [varP f, varP g, varP z, varP t] $
         appsE [ varE appEndoValName
               , appsE [ varE getDualValName
                       , appsE [ makeBifoldMap name, foldFun f, foldFun g, varE t]
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
deriveBitraversable = deriveBiClass Bitraversable

-- | Generates a lambda expression which behaves like 'bitraverse' (without requiring a
-- 'Bitraversable' instance).
makeBitraverse :: Name -> Q Exp
makeBitraverse = makeBiFun Bitraverse

-- | Generates a lambda expression which behaves like 'bisequenceA' (without requiring a
-- 'Bitraversable' instance).
makeBisequenceA :: Name -> Q Exp
makeBisequenceA name = appsE [ makeBitraverse name
                             , varE idValName
                             , varE idValName
                             ]

-- | Generates a lambda expression which behaves like 'bimapM' (without requiring a
-- 'Bitraversable' instance).
makeBimapM :: Name -> Q Exp
makeBimapM name = do
    f <- newName "f"
    g <- newName "g"
    lamE [varP f, varP g] . infixApp (varE unwrapMonadValName) (varE composeValName) $
                            appsE [makeBitraverse name, wrapMonadExp f, wrapMonadExp g]
  where
    wrapMonadExp :: Name -> Q Exp
    wrapMonadExp n = infixApp (conE wrapMonadDataName) (varE composeValName) (varE n)

-- | Generates a lambda expression which behaves like 'bisequence' (without requiring a
-- 'Bitraversable' instance).
makeBisequence :: Name -> Q Exp
makeBisequence name = appsE [ makeBimapM name
                            , varE idValName
                            , varE idValName
                            ]

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

-- | Derive a class instance declaration (depending on the BiClass argument's value).
deriveBiClass :: BiClass -> Name -> Q [Dec]
deriveBiClass biClass tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> deriveBiClassPlainTy biClass tyConName
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> deriveBiClassDataFamInst biClass tyConName
        FamilyI (FamilyD DataFam _ _ _) _ ->
            error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
        FamilyI (FamilyD TypeFam _ _ _) _ ->
            error $ ns ++ "Cannot use a type family name."
        _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
        DataConI{} -> dataConIError
        _          -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Data.Bifunctor.TH.deriveBiClass: "

-- | Generates a class instance declaration for a plain type constructor.
deriveBiClassPlainTy :: BiClass -> Name -> Q [Dec]
deriveBiClassPlainTy biClass tyConName =
    withTyCon tyConName fromCons
  where
    className :: Name
    className = biClassName biClass

    fromCons :: Cxt -> [TyVarBndr] -> [Con] -> Q [Dec]
    fromCons ctxt tvbs cons = (:[]) `fmap`
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (biFunDecs biClass droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypePlainTy biClass tyConName ctxt tvbs

#if MIN_VERSION_template_haskell(2,7,0)
-- | Generates a class instance declaration for a data family instance constructor.
deriveBiClassDataFamInst :: BiClass -> Name -> Q [Dec]
deriveBiClassDataFamInst biClass dataFamInstName =
    withDataFamInstCon dataFamInstName fromDec
  where
    className :: Name
    className = biClassName biClass

    fromDec :: [TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q [Dec]
    fromDec famTvbs ctxt parentName instTys cons = (:[]) `fmap`
        instanceD (return instanceCxt)
                  (return $ AppT (ConT className) instanceType)
                  (biFunDecs biClass droppedNbs cons)
      where
        (instanceCxt, instanceType, droppedNbs) =
            cxtAndTypeDataFamInstCon biClass parentName ctxt famTvbs instTys
#endif

-- | Generates a declaration defining the primary function(s) corresponding to a
-- particular class (bimap for Bifunctor, bifoldr and bifoldMap for Bifoldable, and
-- bitraverse for Bitraversable).
--
-- For why both bifoldr and bifoldMap are derived for Bifoldable, see Trac #7436.
biFunDecs :: BiClass -> [NameBase] -> [Con] -> [Q Dec]
biFunDecs biClass nbs cons = map makeFunD $ biClassToFuns biClass
  where
    makeFunD :: BiFun -> Q Dec
    makeFunD biFun =
        funD (biFunName biFun)
             [ clause []
                      (normalB $ makeBiFunForCons biFun nbs cons)
                      []
             ]

-- | Generates a lambda expression which behaves like the BiFun argument.
makeBiFun :: BiFun -> Name -> Q Exp
makeBiFun biFun tyConName = do
    info <- reify tyConName
    case info of
        TyConI{} -> withTyCon tyConName $ \ctxt tvbs decs ->
            let !nbs = thd3 $ cxtAndTypePlainTy (biFunToClass biFun) tyConName ctxt tvbs
            in makeBiFunForCons biFun nbs decs
#if MIN_VERSION_template_haskell(2,7,0)
        DataConI{} -> withDataFamInstCon tyConName $ \famTvbs ctxt parentName instTys cons ->
            let !nbs = thd3 $ cxtAndTypeDataFamInstCon (biFunToClass biFun) parentName ctxt famTvbs instTys
            in makeBiFunForCons biFun nbs cons
        FamilyI (FamilyD DataFam _ _ _) _ ->
            error $ ns ++ "Cannot use a data family name. Use a data family instance constructor instead."
        FamilyI (FamilyD TypeFam _ _ _) _ ->
            error $ ns ++ "Cannot use a type family name."
        _ -> error $ ns ++ "The name must be of a plain type constructor or data family instance constructor."
#else
        DataConI{} -> dataConIError
        _          -> error $ ns ++ "The name must be of a plain type constructor."
#endif
  where
    ns :: String
    ns = "Data.Bifunctor.TH.makeBiFun: "

-- | Generates a lambda expression for the given constructors.
-- All constructors must be from the same type.
makeBiFunForCons :: BiFun -> [NameBase] -> [Con] -> Q Exp
makeBiFunForCons biFun nbs cons = do
    argNames <- mapM newName $ catMaybes [ Just "f"
                                         , Just "g"
                                         , guard (biFun == Bifoldr) >> Just "z"
                                         , Just "value"
                                         ]
    let (maps,others) = splitAt 2 argNames
        z             = head others -- If we're deriving bifoldr, this will be well defined
                                    -- and useful. Otherwise, it'll be ignored.
        value         = last others
        tvis          = zip nbs maps
    lamE (map varP argNames)
        . appsE
        $ [ varE $ biFunConstName biFun
          , if null cons
               then appE (varE errorValName)
                         (stringE $ "Void " ++ nameBase (biFunName biFun))
               else caseE (varE value)
                          (map (makeBiFunForCon biFun z tvis) cons)
          ] ++ map varE argNames

-- | Generates a lambda expression for a single constructor.
makeBiFunForCon :: BiFun -> Name -> [TyVarInfo] -> Con -> Q Match
makeBiFunForCon biFun z tvis (NormalC conName tys) = do
    args <- newNameList "arg" $ length tys
    let argTys = map snd tys
    makeBiFunForArgs biFun z tvis conName argTys args
makeBiFunForCon biFun z tvis (RecC conName tys) = do
    args <- newNameList "arg" $ length tys
    let argTys = map thd3 tys
    makeBiFunForArgs biFun z tvis conName argTys args
makeBiFunForCon biFun z tvis (InfixC (_, argTyL) conName (_, argTyR)) = do
    argL <- newName "argL"
    argR <- newName "argR"
    makeBiFunForArgs biFun z tvis conName [argTyL, argTyR] [argL, argR]
makeBiFunForCon biFun z tvis (ForallC tvbs faCxt con) =
    if any (`predMentionsNameBase` map fst tvis) faCxt
         && not (allowExQuant (biFunToClass biFun))
       then existentialContextError (constructorName con)
       else makeBiFunForCon biFun z (removeForalled tvbs tvis) con

-- | Generates a lambda expression for a single constructor's arguments.
makeBiFunForArgs :: BiFun
                 -> Name
                 -> [TyVarInfo]
                 -> Name
                 -> [Type]
                 -> [Name]
                 ->  Q Match
makeBiFunForArgs biFun z tvis conName tys args =
    let mappedArgs :: [Q Exp]
        mappedArgs = zipWith (makeBiFunForArg biFun tvis conName) tys args
     in match (conP conName $ map varP args)
              (normalB $ biFunCombine biFun conName z mappedArgs)
              []

-- | Generates a lambda expression for a single argument of a constructor.
makeBiFunForArg :: BiFun
                -> [TyVarInfo]
                -> Name
                -> Type
                -> Name
                -> Q Exp
makeBiFunForArg biFun tvis conName ty tyExpName = do
    ty' <- expandSyn ty
    makeBiFunForArg' biFun tvis conName ty' tyExpName

-- | Generates a lambda expression for a single argument of a constructor, after
-- expanding all type synonyms.
makeBiFunForArg' :: BiFun
                 -> [TyVarInfo]
                 -> Name
                 -> Type
                 -> Name
                 -> Q Exp
makeBiFunForArg' biFun tvis conName ty tyExpName =
    makeBiFunForType biFun tvis conName True ty `appE` varE tyExpName

-- | Generates a lambda expression for a specific type.
makeBiFunForType :: BiFun
                 -> [TyVarInfo]
                 -> Name
                 -> Bool
                 -> Type
                 -> Q Exp
makeBiFunForType biFun tvis conName covariant (VarT tyName) =
    case lookup (NameBase tyName) tvis of
         Just mapName ->
              varE $ if covariant
                        then mapName
                        else contravarianceError conName
         Nothing -> biFunTriv biFun
makeBiFunForType biFun tvis conName covariant (SigT ty _) =
    makeBiFunForType biFun tvis conName covariant ty
makeBiFunForType biFun tvis conName covariant (ForallT tvbs _ ty)
    = makeBiFunForType biFun (removeForalled tvbs tvis) conName covariant ty
makeBiFunForType biFun tvis conName covariant ty =
    let tyCon  :: Type
        tyArgs :: [Type]
        tyCon:tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min 2 $ length tyArgs

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNameBases :: [NameBase]
        tyVarNameBases = map fst tvis

        mentionsTyArgs :: Bool
        mentionsTyArgs = any (`mentionsNameBase` tyVarNameBases) tyArgs

        makeBiFunTuple :: Type -> Name -> Q Exp
        makeBiFunTuple fieldTy fieldName =
            makeBiFunForType biFun tvis conName covariant fieldTy `appE` varE fieldName

     in case tyCon of
             ArrowT | not (allowFunTys (biFunToClass biFun)) -> noFunctionsError conName
                    | mentionsTyArgs ->
                 let [argTy, resTy] = tyArgs
                  in do x <- newName "x"
                        b <- newName "b"
                        lamE [varP x, varP b] $
                             covBiFun covariant resTy `appE` (varE x `appE`
                                (covBiFun (not covariant) argTy `appE` varE b))
                          where
                            covBiFun :: Bool -> Type -> Q Exp
                            covBiFun cov t = makeBiFunForType biFun tvis conName cov t
--                       [| \x b ->
--                          $(makeBiFunForType biFun tvis conName covariant resTy)
--                          (x ($(makeBiFunForType biFun tvis conName (not covariant) argTy) b))
--                       |]
             TupleT n | n > 0 && mentionsTyArgs -> do
                 args <- mapM newName $ catMaybes [ Just "x"
                                                  , guard (biFun == Bifoldr) >> Just "z"
                                                  ]
                 xs <- newNameList "tup" n

                 let x = head args
                     z = last args
                 lamE (map varP args) $ caseE (varE x)
                      [ match (tupP $ map varP xs)
                              (normalB $ biFunCombine biFun
                                                      (tupleDataName n)
                                                      z
                                                      (zipWith makeBiFunTuple tyArgs xs)
                              )
                              []
                      ]
             _ -> do
                 itf <- isTyFamily tyCon
                 if any (`mentionsNameBase` tyVarNameBases) lhsArgs || (itf && mentionsTyArgs)
                      then outOfPlaceTyVarError conName tyVarNameBases
                      else if any (`mentionsNameBase` tyVarNameBases) rhsArgs
                           then biFunApp biFun . appsE $
                                ( varE (fromJust $ biFunArity biFun numLastArgs)
                                : map (makeBiFunForType biFun tvis conName covariant) rhsArgs
                                )
                           else biFunTriv biFun

-------------------------------------------------------------------------------
-- Template Haskell reifying and AST manipulation
-------------------------------------------------------------------------------

-- | Extracts a plain type constructor's information.
withTyCon :: Name
          -> (Cxt -> [TyVarBndr] -> [Con] -> Q a)
          -> Q a
withTyCon name f = do
    info <- reify name
    case info of
        TyConI dec ->
            case dec of
                DataD    ctxt _ tvbs cons _ -> f ctxt tvbs cons
                NewtypeD ctxt _ tvbs con  _ -> f ctxt tvbs [con]
                other -> error $ ns ++ "Unsupported type " ++ show other ++ ". Must be a data type or newtype."
        _ -> error $ ns ++ "The name must be of a plain type constructor."
  where
    ns :: String
    ns = "Data.Bifunctor.TH.withTyCon: "

#if MIN_VERSION_template_haskell(2,7,0)
-- | Extracts a data family name's information.
withDataFam :: Name
            -> ([TyVarBndr] -> [Dec] -> Q a)
            -> Q a
withDataFam name f = do
    info <- reify name
    case info of
        FamilyI (FamilyD DataFam _ tvbs _) decs -> f tvbs decs
        FamilyI (FamilyD TypeFam _ _    _) _    ->
            error $ ns ++ "Cannot use a type family name."
        other -> error $ ns ++ "Unsupported type " ++ show other ++ ". Must be a data family name."
  where
    ns :: String
    ns = "Data.Bifunctor.TH.withDataFam: "

-- | Extracts a data family instance constructor's information.
withDataFamInstCon :: Name
                   -> ([TyVarBndr] -> Cxt -> Name -> [Type] -> [Con] -> Q a)
                   -> Q a
withDataFamInstCon dficName f = do
    dficInfo <- reify dficName
    case dficInfo of
        DataConI _ _ parentName _ -> do
            parentInfo <- reify parentName
            case parentInfo of
                FamilyI (FamilyD DataFam _ _ _) _ -> withDataFam parentName $ \famTvbs decs ->
                    let sameDefDec = flip find decs $ \dec ->
                          case dec of
                              DataInstD    _ _ _ cons' _ -> any ((dficName ==) . constructorName) cons'
                              NewtypeInstD _ _ _ con   _ -> dficName == constructorName con
                              _ -> error $ ns ++ "Must be a data or newtype instance."

                        (ctxt, instTys, cons) = case sameDefDec of
                              Just (DataInstD    ctxt' _ instTys' cons' _) -> (ctxt', instTys', cons')
                              Just (NewtypeInstD ctxt' _ instTys' con   _) -> (ctxt', instTys', [con])
                              _ -> error $ ns ++ "Could not find data or newtype instance constructor."

                    in f famTvbs ctxt parentName instTys cons
                _ -> error $ ns ++ "Data constructor " ++ show dficName ++ " is not from a data family instance."
        other -> error $ ns ++ "Unsupported type " ++ show other ++ ". Must be a data family instance constructor."
  where
    ns :: String
    ns = "Data.Bifunctor.TH.withDataFamInstCon: "
#endif

-- | Deduces the instance context, instance head, and eta-reduced type variables
-- for a plain data type constructor.
cxtAndTypePlainTy :: BiClass     -- Bifunctor, Bifoldable, or Bitraversable
                  -> Name        -- The datatype's name
                  -> Cxt         -- The datatype context
                  -> [TyVarBndr] -- The type variables
                  -> (Cxt, Type, [NameBase])
cxtAndTypePlainTy biClass tyConName dataCxt tvbs =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError biClass tyConName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError tyConName instanceType
    else (instanceCxt, instanceType, droppedNbs)
  where
    instanceCxt :: Cxt
    instanceCxt = mapMaybe (applyConstraint biClass) remaining

    instanceType :: Type
    instanceType = applyTyCon tyConName $ map (VarT . tvbName) remaining

    remainingLength :: Int
    remainingLength = length tvbs - 2

    remaining, dropped :: [TyVarBndr]
    (remaining, dropped) = splitAt remainingLength tvbs

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind dropped

    droppedNbs :: [NameBase]
    droppedNbs = map (NameBase . tvbName) dropped

#if MIN_VERSION_template_haskell(2,7,0)
-- | Deduces the instance context, instance head, and eta-reduced type variables
-- for a data family instance constructor.
cxtAndTypeDataFamInstCon :: BiClass     -- Bifunctor, Bifoldable, or Bitraversable
                         -> Name        -- The data family name
                         -> Cxt         -- The datatype context
                         -> [TyVarBndr] -- The data family declaration's type variables
                         -> [Type]      -- The data family instance types
                         -> (Cxt, Type, [NameBase])
cxtAndTypeDataFamInstCon biClass parentName dataCxt famTvbs instTysAndKinds =
    if remainingLength < 0 || not (wellKinded droppedKinds) -- If we have enough well-kinded type variables
       then derivingKindError biClass parentName
    else if any (`predMentionsNameBase` droppedNbs) dataCxt -- If the last type variable(s) are mentioned in a datatype context
       then datatypeContextError parentName instanceType
    else if canEtaReduce remaining dropped -- If it is safe to drop the type variables
       then (instanceCxt, instanceType, droppedNbs)
    else etaReductionError instanceType
  where
    instanceCxt :: Cxt
    instanceCxt = mapMaybe (applyConstraint biClass) lhsTvbs

    -- We need to make sure that type variables in the instance head which have
    -- constraints aren't poly-kinded, e.g.,
    --
    -- @
    -- instance Bifunctor f => Bifunctor (Foo (f :: k)) where
    -- @
    --
    -- To do this, we remove every kind ascription (i.e., strip off every 'SigT').
    instanceType :: Type
    instanceType = applyTyCon parentName
                 $ map unSigT remaining

    remainingLength :: Int
    remainingLength = length famTvbs - 2

    remaining, dropped :: [Type]
    (remaining, dropped) = splitAt remainingLength rhsTypes

    droppedKinds :: [Kind]
    droppedKinds = map tvbKind . snd $ splitAt remainingLength famTvbs

    droppedNbs :: [NameBase]
    droppedNbs = map varTToNameBase dropped

    -- We need to be mindful of an old GHC bug which causes kind variables to appear in
    -- @instTysAndKinds@ (as the name suggests) if
    --
    --   (1) @PolyKinds@ is enabled
    --   (2) either GHC 7.6 or 7.8 is being used (for more info, see Trac #9692).
    --
    -- Since Template Haskell doesn't seem to have a mechanism for detecting which
    -- language extensions are enabled, we do the next-best thing by counting
    -- the number of distinct kind variables in the data family declaration, and
    -- then dropping that number of entries from @instTysAndKinds@.
    instTypes :: [Type]
    instTypes =
# if __GLASGOW_HASKELL__ >= 710 || !(MIN_VERSION_template_haskell(2,8,0))
        instTysAndKinds
# else
        drop (Set.size . Set.unions $ map (distinctKindVars . tvbKind) famTvbs)
             instTysAndKinds
# endif

    lhsTvbs :: [TyVarBndr]
    lhsTvbs = map (uncurry replaceTyVarName)
            . filter (isTyVar . snd)
            . take remainingLength
            $ zip famTvbs rhsTypes

    -- In GHC 7.8, only the @Type@s up to the rightmost non-eta-reduced type variable
    -- in @instTypes@ are provided (as a result of a bug reported in Trac #9692). This
    -- is pretty inconvenient, as it makes it impossible to come up with the correct
    -- instance types in some cases. For example, consider the following code:
    --
    -- @
    -- data family Foo a b c
    -- data instance Foo Int y z = Foo Int y z
    -- $(deriveBifunctor 'Foo)
    -- @
    --
    -- Due to the aformentioned bug, Template Haskell doesn't tell us the names of
    -- either of type variables in the data instance (@y@ and @z@). As a result, we
    -- won't know to which fields of the 'Foo' constructor to apply the map functions,
    -- which will result in an incorrect instance. Urgh.
    --
    -- A workaround is to ensure that you use the exact same type variables, in the
    -- exact same order, in the data family declaration and any data or newtype
    -- instances:
    --
    -- @
    -- data family Foo a b c
    -- data instance Foo Int b c = Foo Int b c
    -- $(deriveBifunctor 'Foo)
    -- @
    --
    -- Thankfully, other versions of GHC don't seem to have this bug.
    rhsTypes :: [Type]
    rhsTypes =
# if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
            instTypes ++ map tvbToType
                             (drop (length instTypes)
                                   famTvbs)
# else
            instTypes
# endif
#endif

-- | Given a TyVarBndr, apply a certain constraint to it, depending on its kind.
applyConstraint :: BiClass -> TyVarBndr -> Maybe Pred
applyConstraint _       (PlainTV  _)         = Nothing
applyConstraint biClass (KindedTV name kind) = do
    constraint <- biClassConstraint biClass $ numKindArrows kind
    if canRealizeKindStarChain kind
       then Just $ applyClass constraint name
       else Nothing

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

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> a
etaReductionError instanceType = error $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

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
outOfPlaceTyVarError :: Name -> [NameBase] -> a
outOfPlaceTyVarError conName tyVarNames = error
    . showString "Constructor ‘"
    . showString (nameBase conName)
    . showString "‘ must use the type variable(s) "
    . showsPrec 0 tyVarNames
    . showString " only in the last argument(s) of a data type"
    $ ""

#if !(MIN_VERSION_template_haskell(2,7,0))
-- | Template Haskell didn't list all of a data family's instances upon reification
-- until template-haskell-2.7.0.0, which is necessary for a derived instance to work.
dataConIError :: a
dataConIError = error
    . showString "Cannot use a data constructor."
    . showString "\n\t(Note: if you are trying to derive for a data family instance,"
    . showString "\n\tuse GHC >= 7.4 instead.)"
    $ ""
#endif

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

biFunCombine :: BiFun -> Name -> Name -> [Q Exp] -> Q Exp
biFunCombine Bimap      = bimapCombine
biFunCombine Bifoldr    = bifoldrCombine
biFunCombine BifoldMap  = bifoldMapCombine
biFunCombine Bitraverse = bitraverseCombine

bimapCombine :: Name -> Name -> [Q Exp] -> Q Exp
bimapCombine conName _ = foldl' appE (conE conName)

bifoldrCombine :: Name -> Name -> [Q Exp] -> Q Exp
bifoldrCombine _ zName = foldr appE (varE zName)

bifoldMapCombine :: Name -> Name -> [Q Exp] -> Q Exp
bifoldMapCombine _ _ [] = varE memptyValName
bifoldMapCombine _ _ es = foldr1 (appE . appE (varE mappendValName)) es

bitraverseCombine :: Name -> Name -> [Q Exp] -> Q Exp
bitraverseCombine conName _ [] = varE pureValName `appE` conE conName
bitraverseCombine conName _ (e:es) =
    foldl' (flip infixApp $ varE apValName)
        (appsE [varE fmapValName, conE conName, e]) es
