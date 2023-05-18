module Primer.Typecheck.Kindcheck (
  KindError (..),
  checkKind,
  synthKind,
  Type,
  consistentKinds,
) where

import Foreword

import Primer.Core.Type (
  Kind (KFun, KHole, KType),
  Type (TApp, TEmptyHole, TFun),
 )

data KindError
  = InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  deriving stock (Eq, Show, Read)

-- | A shorthand for the constraints needed when kindchecking
type KindM m =
  ( Monad m
  , MonadError KindError m -- can throw kind errors
  )

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
synthKind :: KindM m => Type -> m (Kind, Type)
synthKind = \case
  TEmptyHole -> pure (KHole, TEmptyHole)
  TFun a b -> do
    a' <- checkKind KType a
    b' <- checkKind KType b
    pure (KType, TFun a' b')
  TApp s t -> do
    (k, s') <- synthKind s
    case matchArrowKind k of
      Nothing -> throwError $ KindDoesNotMatchArrow k
      Just (k1, k2) -> checkKind k1 t >>= \t' -> pure (k2, TApp s' t')

checkKind :: KindM m => Kind -> Type -> m Type
checkKind k t = do
  (k', t') <- synthKind t
  if consistentKinds k k'
    then pure t'
    else throwError $ InconsistentKinds k k'

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
