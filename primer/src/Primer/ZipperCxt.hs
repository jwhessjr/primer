module Primer.ZipperCxt (
  localVariablesInScopeExpr,
  extractLocalsExprZ,
  extractLocalsTypeZ,
  variablesInScopeTy,
  ShadowedVarsExpr (M),
  ShadowedVarsTy (N),
) where

import Foreword

import Data.Set qualified as Set
import Optics ((%), (^.))
import Primer.Core (
  Bind' (..),
  CaseBranch' (..),
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName),
  Kind (KHole),
  LVarName,
  LocalName (unLocalName),
  Meta (Meta),
  TyVarName,
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  _type,
  _typeMetaLens,
 )
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Typecheck.Utils (maybeTypeOf)
import Primer.Zipper (
  ExprZ,
  FoldAbove,
  TypeZ,
  TypeZip,
  asZipper,
  current,
  foldAbove,
  prior,
  unfocusType,
 )

-- Helper for variablesInScopeExpr: collect variables, most local first,
-- eliding shadowed variables
-- ["shadowed" here means by name, even though even though internally they are
-- in different namespaces; thus a lambda-bound variable "main" will shadow the
-- globally-defined "main"]
data ShadowedVarsExpr
  = M
      -- | Local type variables
      [(TyVarName, Kind)]
      -- | Local term variables
      [(LVarName, Type' ())]
      -- | Global variables
      [(GVarName, Type' ())]
  deriving stock (Eq, Show)

instance Semigroup ShadowedVarsExpr where
  M ty1 tm1 gl1 <> M ty2 tm2 gl2 = M (ty1 <> ty2') (tm1 <> tm2') (gl1 <> gl2')
    where
      names1 =
        Set.fromList (map (unLocalName . fst) ty1)
          <> Set.fromList (map (unLocalName . fst) tm1)
          <> Set.fromList (map (baseName . fst) gl1)
      ty2' = filter (flip Set.notMember names1 . unLocalName . fst) ty2
      tm2' = filter (flip Set.notMember names1 . unLocalName . fst) tm2
      gl2' = filter (flip Set.notMember names1 . baseName . fst) gl2

instance Monoid ShadowedVarsExpr where
  mempty = M mempty mempty mempty

-- | Collect the typing context for the focused node.
-- We do this by walking back up the tree, collecting variables as we cross
-- binders.
-- The first list is local type variables, the second list is local term variables.
-- We remove shadowed variables.
-- Note that type/kind information is extracted from the TypeCache.
localVariablesInScopeExpr ::
  Either ExprZ TypeZ ->
  ([(TyVarName, Kind)], [(LVarName, Type' ())])
localVariablesInScopeExpr exprOrTy =
  let M tyvars tmvars _globs = either extractLocalsExprZ extractLocalsTypeZ exprOrTy
   in (reverse tyvars, reverse tmvars) -- keep most-global first

extractLocalsTypeZ :: TypeZ -> ShadowedVarsExpr
extractLocalsTypeZ z =
  let x = variablesInScopeTy $ z ^. asZipper
      y = unfocusType z
   in -- walkUpExpr will extract binders strictly containing y
      -- (i.e. if y=λs.t, then 's' won't be reported). Since no
      -- construct both contains a type and binds a variable, this
      -- will not miss anything.
      M (reverse x) [] [] <> extractLocalsExprZ y

extractLocalsExprZ :: ExprZ -> ShadowedVarsExpr
extractLocalsExprZ = foldAbove getBoundHere
  where
    getBoundHere :: FoldAbove Expr -> ShadowedVarsExpr
    getBoundHere e = case current e of
      Lam m x _ ->
        let tx = case typeOrHole m of
              TFun _ a _ -> a
              _ -> TEmptyHole ()
         in M [] [(x, tx)] []
      LAM m n _ ->
        let k = case typeOrHole m of
              TForall _ _ a _ -> a
              _ -> KHole
         in M [(n, k)] [] []
      Let _ x e1 _
        | prior e == e1 -> mempty
        | otherwise -> M [] [(x, typeOrHole' $ maybeTypeOf e1)] []
      Letrec _ x _ ty _ -> M [] [(x, forgetTypeMetadata ty)] []
      LetType _ x ty _ -> M [(x, kindOrHoleOf ty)] [] []
      Case _ _ branches ->
        let fromBinding (Bind m n) = (n, typeOrHole m)
            binderss = map (\(CaseBranch _ ns rhs) -> (rhs, map fromBinding ns)) branches
         in mconcat $ map (\(b, binders) -> if b == prior e then M [] binders [] else mempty) binderss
      _ -> mempty

    -- If a node has no type annotation we assign it type TEmptyHole
    typeOrHole :: Meta (Maybe TypeCache) -> Type' ()
    typeOrHole (Meta _ t _) = typeOrHole' t

    typeOrHole' :: Maybe TypeCache -> Type' ()
    typeOrHole' = maybe (TEmptyHole ()) uncache

    -- Extract a Type from a TypeCache
    uncache :: TypeCache -> Type' ()
    uncache (TCSynthed t) = t
    uncache (TCChkedAt t) = t
    uncache (TCEmb TCBoth{tcSynthed = t}) = t

-- If a type has no cached kind we report it has kind KHole
kindOrHoleOf :: Type -> Kind
kindOrHoleOf t = fromMaybe KHole $ t ^. _typeMetaLens % _type

-- Helper for variablesInScopeTy: collect variables, most local first, eliding
-- shadowed vars, as with 'ShadowedVarsExpr'
newtype ShadowedVarsTy = N [(TyVarName, Kind)]
  deriving stock (Eq, Show)

instance Semigroup ShadowedVarsTy where
  N ty1 <> N ty2 = N (ty1 <> ty2')
    where
      names1 = Set.fromList (map fst ty1)
      ty2' = filter (flip Set.notMember names1 . fst) ty2

instance Monoid ShadowedVarsTy where
  mempty = N mempty

-- | As for 'variablesInScopeExpr', but when you are focussed somewhere inside
-- a type, rather than somewhere inside an expr
-- Note that kind information is extracted from the cached kind (for 'TLet')
variablesInScopeTy :: TypeZip -> [(TyVarName, Kind)]
variablesInScopeTy e =
  let N vs = foldAbove getBoundHere e
   in reverse vs -- keep most-global first
  where
    getBoundHere :: FoldAbove Type -> ShadowedVarsTy
    getBoundHere t = case current t of
      TForall _ v k _ -> N [(v, k)]
      TLet _ v t' b
        | prior t == b -> N [(v, kindOrHoleOf t')]
        | otherwise -> mempty
      _ -> mempty
