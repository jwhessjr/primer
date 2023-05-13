module Primer.Typecheck.Kindcheck (
  KindError (..),
  checkKind,
  synthKind,
  Type,
  TypeT,
  KindOrType (..),
  extendLocalCxtTy,
  extendLocalCxtTys,
  lookupLocalTy,
  consistentKinds,
  annotate,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.NestedError (MonadNestedError, throwError')
import Data.Map qualified as Map
import Primer.Core.DSL.Meta (meta')
import Primer.Core.Meta (ID, Meta (Meta), TyVarName, unLocalName)
import Primer.Core.Type (
  Kind (KType),
  Type' (TCon, TEmptyHole, TFun, THole),
 )
import Primer.Name (NameCounter)
import Primer.TypeDef (typeDefKind)
import Primer.Typecheck.Cxt (Cxt (localCxt, smartHoles, typeDefs), KindOrType (K, T), Type)
import Primer.Typecheck.KindError (
  KindError (
    InconsistentKinds,
    KindDoesNotMatchArrow,
    TLetUnsupported,
    TyVarWrongSort,
    UnknownTypeConstructor,
    UnknownTypeVariable
  ),
 )
import Primer.Typecheck.SmartHoles (SmartHoles (NoSmartHoles, SmartHoles))

-- | A shorthand for the constraints needed when kindchecking
type KindM e m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadNestedError KindError e m -- can throw kind errors
  )

type TypeT = Type' (Meta Kind)

lookupLocalTy :: TyVarName -> Cxt -> Either KindError Kind
lookupLocalTy v cxt = case Map.lookup (unLocalName v) $ localCxt cxt of
  Just (K k) -> Right k
  Just (T _) -> Left $ TyVarWrongSort (unLocalName v)
  Nothing -> Left $ UnknownTypeVariable v

extendLocalCxtTy :: (TyVarName, Kind) -> Cxt -> Cxt
extendLocalCxtTy (name, k) cxt = cxt{localCxt = Map.insert (unLocalName name) (K k) (localCxt cxt)}

extendLocalCxtTys :: [(TyVarName, Kind)] -> Cxt -> Cxt
extendLocalCxtTys x cxt = cxt{localCxt = Map.fromList (bimap unLocalName K <$> x) <> localCxt cxt}

-- Synthesise a kind for the given type
synthKind :: KindM e m => Type' (Meta a) -> m (Kind, TypeT)
synthKind = \case
  TEmptyHole m -> pure (KType, TEmptyHole (annotate KType m))
  THole m t -> do
    sh <- asks smartHoles
    (k, t') <- synthKind t
    case sh of
      NoSmartHoles -> pure (KType, THole (annotate KType m) t')
      SmartHoles -> pure (k, t')
  TCon m c -> do
    typeDef <- asks (Map.lookup c . typeDefs)
    case typeDef of
      Nothing -> throwError' $ UnknownTypeConstructor c
      Just def -> let k = typeDefKind def in pure (k, TCon (annotate k m) c)
  TFun m a b -> do
    a' <- checkKind KType a
    b' <- checkKind KType b
    pure (KType, TFun (annotate KType m) a' b')

checkKind :: KindM e m => Kind -> Type' (Meta a) -> m TypeT
checkKind k (THole m t) = do
  -- If we didn't have this special case, we might remove this hole (in a
  -- recursive call), only to reintroduce it again with a different ID
  -- TODO: ugly and duplicated...
  sh <- asks smartHoles
  (k', t') <- synthKind t
  case (consistentKinds k k', sh) of
    (_, NoSmartHoles) -> pure $ THole (annotate KType m) t'
    (True, SmartHoles) -> pure t'
    (False, SmartHoles) -> pure $ THole (annotate KType m) t'
checkKind k t = do
  sh <- asks smartHoles
  (k', t') <- synthKind t
  case (consistentKinds k k', sh) of
    (True, _) -> pure t'
    (False, NoSmartHoles) -> throwError' $ InconsistentKinds k k'
    (False, SmartHoles) -> THole <$> meta' KType <*> pure t'

-- | Extend the metadata of an 'Expr' or 'Type'
-- (usually with a 'TypeCache' or 'Kind')
annotate :: b -> Meta a -> Meta b
annotate t (Meta i _ v) = Meta i t v

consistentKinds :: Kind -> Kind -> Bool
consistentKinds _ _ = True
