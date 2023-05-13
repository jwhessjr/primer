module Primer.Typecheck.Kindcheck (
  KindError (..),
  checkKind,
  synthKind,
  Type,
  TypeT,
  KindOrType (..),
  annotate,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.NestedError (MonadNestedError, throwError')
import Data.Map qualified as Map
import Primer.Core.Meta (ID, Meta (Meta))
import Primer.Core.Type (
  Kind (KType),
  Type' (TCon, TEmptyHole, TFun),
 )
import Primer.Name (NameCounter)
import Primer.TypeDef (typeDefKind)
import Primer.Typecheck.Cxt (Cxt (typeDefs), KindOrType (K, T), Type)
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

-- Synthesise a kind for the given type
synthKind :: KindM e m => Type' (Meta a) -> m (Kind, TypeT)
synthKind = \case
  TEmptyHole m -> pure (KType, TEmptyHole (annotate KType m))
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
checkKind _ t = do
  (_, t') <- synthKind t
  pure t'

-- | Extend the metadata of an 'Expr' or 'Type'
-- (usually with a 'TypeCache' or 'Kind')
annotate :: b -> Meta a -> Meta b
annotate t (Meta i _ v) = Meta i t v
