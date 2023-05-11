-- | Utilities useful across several types of tests.
module Primer.Test.Util (
  ExceptionPredicate,
  primDefs,
  constructTCon,
  constructCon,
  constructRefinedCon,
  tcn,
  vcn,
  gvn,
  zeroIDs,
  zeroTypeIDs,
  clearMeta,
  clearTypeMeta,
  LogMsg,
  isSevereLog,
  testNoSevereLogs,
  failWhenSevereLogs,
) where

import Foreword

import Control.Monad.Log (Severity (Informational), WithSeverity (msgSeverity))
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Hedgehog (MonadTest, (===))
import Optics (over, set, view)
import Primer.Action (
  Action (ConstructCon, ConstructRefinedCon, ConstructTCon),
 )
import Primer.Core (
  Expr',
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID,
  HasMetadata (_metadata),
  ModuleName (ModuleName, unModuleName),
  TyConName,
  Type',
  TypeMeta,
  ValConName,
  Value,
  qualifyName,
  setID,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Core.Utils (exprIDs)
import Primer.Def (DefMap)
import Primer.Log (ConvertLogMessage (convert), PureLogT, runPureLogT)
import Primer.Module (Module (moduleDefs), primitiveModule)
import Primer.Name (Name (unName))
import Primer.Primitives (primitive)

primDefs :: DefMap
primDefs = Map.mapKeys primitive $ moduleDefs primitiveModule

-- impedence mismatch: ConstructTCon takes text, but tChar etc are TyConNames
constructTCon :: TyConName -> Action
constructTCon = ConstructTCon . toQualText

constructCon :: ValConName -> Action
constructCon = ConstructCon . toQualText

constructRefinedCon :: ValConName -> Action
constructRefinedCon = ConstructRefinedCon . toQualText

toQualText :: GlobalName k -> (NonEmpty Text, Text)
toQualText n = (map unName $ unModuleName $ qualifiedModule n, unName $ baseName n)

vcn :: NonEmpty Name -> Name -> ValConName
vcn = qualifyName . ModuleName

tcn :: NonEmpty Name -> Name -> TyConName
tcn = qualifyName . ModuleName

gvn :: NonEmpty Name -> Name -> GVarName
gvn = qualifyName . ModuleName

-- | Replace all 'ID's in an Expr with 0.
zeroIDs :: (HasID a, HasID b) => Expr' a b -> Expr' a b
zeroIDs = set exprIDs 0

-- | Replace all 'ID's in a Type with 0.
zeroTypeIDs :: HasID a => Type' a -> Type' a
zeroTypeIDs = over _typeMeta (setID 0)

-- | Clear the backend-created metadata (IDs and cached types) in the given expression
clearMeta :: Expr' ExprMeta TypeMeta -> Expr' (Maybe Value) (Maybe Value)
clearMeta = over _exprMeta (view _metadata) . over _exprTypeMeta (view _metadata)

-- | Clear the backend-created metadata (IDs and cached types) in the given expression
clearTypeMeta :: Type' TypeMeta -> Type' (Maybe Value)
clearTypeMeta = over _typeMeta (view _metadata)

type ExceptionPredicate e = (e -> Bool)

newtype LogMsg = LogMsg Text
  deriving newtype (Show)

instance Show l => ConvertLogMessage l LogMsg where
  convert = LogMsg . show

isSevereLog :: WithSeverity l -> Bool
isSevereLog l = msgSeverity l < Informational

testNoSevereLogs :: (HasCallStack, MonadTest m, Eq l, Show l) => Seq (WithSeverity l) -> m ()
testNoSevereLogs logs = Seq.filter isSevereLog logs === mempty

failWhenSevereLogs :: (HasCallStack, MonadTest m, Eq l, Show l) => PureLogT (WithSeverity l) m a -> m a
failWhenSevereLogs m = do
  (r, logs) <- runPureLogT m
  testNoSevereLogs logs
  pure r
