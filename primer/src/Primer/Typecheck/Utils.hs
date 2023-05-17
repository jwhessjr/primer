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
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Map qualified as M
import Data.Tuple.Extra (fst3)
import Optics (Lens', view, (%))
import Primer.Core (Expr', TypeCache, _exprMetaLens)
import Primer.Core.Meta (Meta, TyConName, ValConName, _type)
import Primer.Core.Transform (decomposeTAppCon)
import Primer.Core.Type (Kind, Type' (TEmptyHole, THole))
import Primer.Core.Type.Utils (forgetTypeMetadata)
import Primer.Name (NameCounter)
import Primer.Subst (substTySimul)
import Primer.TypeDef (
  ASTTypeDef (astTypeDefConstructors, astTypeDefParameters),
  TypeDef (TypeDefAST, TypeDefPrim),
  TypeDefMap,
  ValCon (valConArgs, valConName),
  typeDefParameters,
 )
import Primer.Typecheck.Cxt (Cxt, typeDefs)

-- We assume that constructor names are unique, returning the first one we find
lookupConstructor :: TypeDefMap -> ValConName -> Maybe (ValCon (), TyConName, ASTTypeDef ())
lookupConstructor tyDefs c =
  let allCons = do
        (tc, TypeDefAST td) <- M.assocs tyDefs
        vc <- astTypeDefConstructors td
        pure (vc, tc, td)
   in find ((== c) . valConName . fst3) allCons

data TypeDefError
  = TDIHoleType -- a type hole
  | TDINotADT -- e.g. a function type etc
  | TDIUnknown TyConName -- not in scope
  | TDINotSaturated -- e.g. @List@ or @List a b@ rather than @List a@

data TypeDefInfo a = TypeDefInfo [Type' a] TyConName (TypeDef ()) -- instantiated parameters, and the typedef (with its name), i.e. [Int] are the parameters for @List Int@

getTypeDefInfo' :: TypeDefMap -> Type' a -> Either TypeDefError (TypeDefInfo a)
getTypeDefInfo' _ (TEmptyHole _) = Left TDIHoleType
getTypeDefInfo' _ (THole _ _) = Left TDIHoleType
getTypeDefInfo' tydefs ty =
  case decomposeTAppCon ty of
    Nothing -> Left TDINotADT
    Just (tycon, params) -> do
      case M.lookup tycon tydefs of
        Nothing -> Left $ TDIUnknown tycon
        Just tydef
          -- this check would be redundant if we were sure that the input type
          -- were of kind KType, alternatively we should do kind checking here
          | length (typeDefParameters tydef) /= length params -> Left TDINotSaturated
          | otherwise -> Right $ TypeDefInfo params tycon tydef

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
    >>= \(TypeDefInfo params tc def) -> case def of
      TypeDefPrim _ -> Left TDINotADT
      TypeDefAST tda -> do
        let defparams = map fst $ astTypeDefParameters tda
            f :: ValCon () -> (ValConName, forall m. MonadFresh NameCounter m => [m (Type' ())])
            -- eta expand to deal with shallow subsumption
            {- HLINT ignore instantiateValCons' "Avoid lambda" -}
            f c = (valConName c, map (\a -> substTySimul (M.fromList $ zip defparams params) (forgetTypeMetadata a)) $ valConArgs c)
        pure (tc, tda, map f $ astTypeDefConstructors tda)

-- | A lens for the type annotation of an 'Expr' or 'ExprT'
_typecache :: Lens' (Expr' (Meta a) b) a
_typecache = _exprMetaLens % _type

-- | Get the type of an 'ExprT'
typeOf :: Expr' (Meta TypeCache) (Meta Kind) -> TypeCache
typeOf = view _typecache
