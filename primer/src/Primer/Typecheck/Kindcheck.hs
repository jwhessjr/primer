module Primer.Typecheck.Kindcheck (
  KindError (..),
  checkKind,
  synthKind,
  Type,
  TypeT,
  KindOrType (..),
  extendLocalCxtTys,
  lookupLocalTy,
  consistentKinds,
  annotate,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Primer.Core.Meta (ID, Meta (Meta), TyConName,TyVarName, unLocalName)
import Primer.Core.Type (
  Kind (KFun, KHole, KType),
  Type' (TApp, TEmptyHole, TFun),
 )
import Primer.Name (Name, NameCounter)
import Primer.Typecheck.Cxt (Cxt (localCxt), KindOrType (K, T), Type)


data KindError
  = UnknownTypeVariable TyVarName
  | TyVarWrongSort Name -- term var instead of type var
  | UnknownTypeConstructor TyConName
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  | -- | We currently cannot typecheck a let inside a type,
    -- they should only transiently appear in evaluation, as explicit substitutions.
    TLetUnsupported
  deriving stock (Eq, Show, Read)

-- | A shorthand for the constraints needed when kindchecking
type KindM m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadError KindError m -- can throw kind errors
  )

type TypeT = Type' (Meta Kind)

lookupLocalTy :: TyVarName -> Cxt -> Either KindError Kind
lookupLocalTy v cxt = case Map.lookup (unLocalName v) $ localCxt cxt of
  Just (K k) -> Right k
  Just (T _) -> Left $ TyVarWrongSort (unLocalName v)
  Nothing -> Left $ UnknownTypeVariable v

extendLocalCxtTys :: [(TyVarName, Kind)] -> Cxt -> Cxt
extendLocalCxtTys x cxt = cxt{localCxt = Map.fromList (bimap unLocalName K <$> x) <> localCxt cxt}

-- Synthesise a kind for the given type
-- TypeHoles are always considered to have kind KHole - a kind hole.
-- When SmartHoles is on, we essentially remove all holes, and re-insert where
-- necessary.
-- However, we take care not to remove a non-empty hole only to immediately
-- re-insert it, since this would needlessly change its ID, resulting in
-- problems if an action left the cursor on such a hole: "lost ID after
-- typechecking". For example, consider (numbers are denoting IDs inside the
-- metadata)
--   synthKind $ TApp 0 (THole 1 (TCon 2 Bool)) t
-- If we removed the hole, we would then note that Bool does not have an arrow
-- kind, and so wrap it in a hole again, returning something like
--   TApp 0 (THole 3 (TCon 2 Bool)) t
-- A similar thing would happen with
--   synthKind $ TApp 0 (TCon 1 List) (THole 2 (TCon 3 List))
-- because we do not have checkKind KType List
synthKind :: KindM m => Type' (Meta a) -> m (Kind, TypeT)
synthKind = \case
  TEmptyHole m -> pure (KHole, TEmptyHole (annotate KHole m))
  TFun m a b -> do
    a' <- checkKind KType a
    b' <- checkKind KType b
    pure (KType, TFun (annotate KType m) a' b')
  TApp m s t -> do
    (k, s') <- synthKind s
    case matchArrowKind k of
      Nothing -> throwError $ KindDoesNotMatchArrow k
      Just (k1, k2) -> checkKind k1 t >>= \t' -> pure (k2, TApp (annotate k2 m) s' t')

checkKind :: KindM m => Kind -> Type' (Meta a) -> m TypeT
checkKind k t = do
  (k', t') <- synthKind t
  if consistentKinds k k'
    then pure t'
    else throwError $ InconsistentKinds k k'

-- | Extend the metadata of an 'Expr' or 'Type'
-- (usually with a 'TypeCache' or 'Kind')
annotate :: b -> Meta a -> Meta b
annotate t (Meta i _ v) = Meta i t v

matchArrowKind :: Kind -> Maybe (Kind, Kind)
matchArrowKind KHole = pure (KHole, KHole)
matchArrowKind KType = Nothing
matchArrowKind (KFun k1 k2) = pure (k1, k2)

consistentKinds :: Kind -> Kind -> Bool
consistentKinds KHole _ = True
consistentKinds _ KHole = True
consistentKinds KType KType = True
consistentKinds (KFun k1 k2) (KFun k1' k2') = consistentKinds k1 k1' && consistentKinds k2 k2'
consistentKinds _ _ = False
