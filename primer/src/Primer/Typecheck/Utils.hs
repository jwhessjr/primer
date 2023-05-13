{-# LANGUAGE ImpredicativeTypes #-}

module Primer.Typecheck.Utils (
  TypeDefError (..),
  TypeDefInfo (..),
  getTypeDefInfo',
  instantiateValCons,
  instantiateValCons',
  lookupConstructor,
  typeOf,
  _typecache,
  getGlobalNames,
  getGlobalBaseNames,
) where

import Foreword

import Control.Arrow ((&&&))
import Control.Monad.Fresh (MonadFresh)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Set qualified as S
import Optics (Lens', view, (%))
import Primer.Core (Expr', GlobalName (baseName, qualifiedModule), ModuleName, TypeCache, _exprMetaLens)
import Primer.Core.Meta (Meta, TyConName, ValConName, _type)
import Primer.Core.Type (Kind, Type' (TCon, TEmptyHole))
import Primer.Name (Name, NameCounter)
import Primer.TypeDef (
  ASTTypeDef,
  TypeDef (TypeDefAST),
  TypeDefMap,
  ValCon,
 )
import Primer.Typecheck.Cxt (Cxt, globalCxt, typeDefs)

-- We assume that constructor names are unique, returning the first one we find
lookupConstructor :: TypeDefMap -> ValConName -> Maybe (ValCon (), TyConName, ASTTypeDef ())
lookupConstructor _tyDefs _c = Nothing

data TypeDefError
  = TDIHoleType -- a type hole
  | TDINotADT -- e.g. a function type etc
  | TDIUnknown TyConName -- not in scope
  | TDINotSaturated -- e.g. @List@ or @List a b@ rather than @List a@

data TypeDefInfo a = TypeDefInfo [Type' a] TyConName (TypeDef ()) -- instantiated parameters, and the typedef (with its name), i.e. [Int] are the parameters for @List Int@

getTypeDefInfo' :: TypeDefMap -> Type' a -> Either TypeDefError (TypeDefInfo a)
getTypeDefInfo' _ (TEmptyHole _) = Left TDIHoleType
getTypeDefInfo' tydefs (TCon _ tycon) =
      case M.lookup tycon tydefs of
        Nothing -> Left $ TDIUnknown tycon
        Just tydef
          -- this check would be redundant if we were sure that the input type
          -- were of kind KType, alternatively we should do kind checking here
          | otherwise -> Right $ TypeDefInfo [] tycon tydef
getTypeDefInfo' _ _ = Left TDINotADT

-- | Takes a particular instance of a parameterised type (e.g. @List Nat@), and
-- extracts both both the raw typedef (e.g. @List a = Nil | Cons a (List a)@)
-- and the constructors with instantiated argument types
-- (e.g. @Nil : List Nat ; Cons : Nat -> List Nat -> List Nat@)
instantiateValCons ::
  (MonadFresh NameCounter m, MonadReader Cxt m) =>
  Type' () ->
  m (Either TypeDefError (TyConName, ASTTypeDef (), [(ValConName, [Type' ()])]))
instantiateValCons t = do
  tds <- asks typeDefs
  let instCons = instantiateValCons' tds t
      -- Because @(,,) a b@ does not have a Traversable instance
      -- we reassociate so we use the one of @(,) a@
      reassoc (a, b, c) = ((a, b), c)
      reassoc' ((a, b), c) = (a, b, c)
      sequence4 =
        fmap (getCompose . getCompose . getCompose . getCompose)
          . sequence
          . Compose
          . Compose
          . Compose
          . Compose
  -- We eta-expand here to deal with simplified subsumption
  {- HLINT ignore instantiateValCons "Use id" -}
  fmap (fmap reassoc') $ sequence4 $ fmap (fmap (fmap $ fmap $ \x -> x) . reassoc) instCons

-- | As 'instantiateValCons', but pulls out the relevant bits of the monadic
-- context into an argument
instantiateValCons' ::
  TypeDefMap ->
  Type' () ->
  Either TypeDefError (TyConName, ASTTypeDef (), [(ValConName, forall m. MonadFresh NameCounter m => [m (Type' ())])])
instantiateValCons' tyDefs t =
  getTypeDefInfo' tyDefs t
    >>= \(TypeDefInfo _params tc def) -> case def of
      TypeDefAST tda -> pure (tc, tda, [])

-- | A lens for the type annotation of an 'Expr' or 'ExprT'
_typecache :: Lens' (Expr' (Meta a) b) a
_typecache = _exprMetaLens % _type

-- | Get the type of an 'ExprT'
typeOf :: Expr' (Meta TypeCache) (Meta Kind) -> TypeCache
typeOf = view _typecache

-- Helper to create fresh names
getGlobalNames :: MonadReader Cxt m => m (S.Set (ModuleName, Name))
getGlobalNames = do
  tyDefs <- asks typeDefs
  topLevel <- asks $ S.fromList . map f . M.keys . globalCxt
  let ctors =
        Map.foldMapWithKey
          ( \t _def ->
              S.fromList [f t]
          )
          tyDefs
  pure $ S.union topLevel ctors
  where
    f = qualifiedModule &&& baseName

getGlobalBaseNames :: MonadReader Cxt m => m (S.Set Name)
getGlobalBaseNames = S.map snd <$> getGlobalNames
