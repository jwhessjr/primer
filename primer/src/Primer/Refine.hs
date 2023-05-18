module Primer.Refine (refine, Inst (..)) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Set qualified as Set
import Primer.Core.Meta (ID, TyVarName)
import Primer.Core.Type (Kind, Type' (TFun))
import Primer.Name (NameCounter)
import Primer.Typecheck.Cxt (Cxt)
import Primer.Typecheck.Kindcheck qualified as TC
import Primer.Unification (InternalUnifyError, unify)

data Inst
  = InstApp TC.Type
  | InstAPP TC.Type
  | InstUnconstrainedAPP TyVarName Kind
  deriving stock (Show, Eq)

-- | Given a target type @T@ and a source type @S@, find an instantiation @I@
-- so that if @e ∈ S@, then @e I ∈ T' ~ T@
-- Here,
--
--  * @e (InstApp ty)@ represents "apply to any @t@ st @ty ∋ t@"
--  * @e (InstAPP ty)@ represents "apply to the type @ty@: @e \@ty@"
--  * @e (InstUnconstrainedAPP _ k)@ represents "apply to some type of kind @k@, but we don't care what"
--
-- The names in @InstUnconstrainedAPP@s are all unique, and they scope over all
-- the @Inst@s to the right, as well as the returned @Type@.
refine ::
  forall m.
  (MonadFresh ID m, MonadFresh NameCounter m, MonadError InternalUnifyError m) =>
  -- | only care about local type vars and typedefs
  Cxt ->
  TC.Type ->
  TC.Type ->
  m (Maybe ([Inst], TC.Type))
refine cxt tgtTy srcTy = go [] srcTy
  where
    go :: [Either TC.Type (TyVarName, Kind)] -> TC.Type -> m (Maybe ([Inst], TC.Type))
    go instantiation tmTy =
      let cxt' = extendCxtTys (rights instantiation) cxt
          uvs = Set.fromList $ map fst $ rights instantiation
       in unify cxt' uvs tgtTy tmTy >>= \case
            Just _sub -> pure $ Just ([], tmTy)
            Nothing -> case tmTy of
              TFun _ s t -> go (Left s : instantiation) t
              _ -> pure Nothing

-- NB: this assumes the list is ordered st the /last/ element is most global
extendCxtTys :: [(TyVarName, Kind)] -> Cxt -> Cxt
extendCxtTys = TC.extendLocalCxtTys . reverse
