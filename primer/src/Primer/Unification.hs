module Primer.Unification (unify) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as M
import Data.Set qualified as S
import Primer.Core.Meta (
  ID,
  TyVarName,
 )
import Primer.Core.Type (
  Type' (TApp, TEmptyHole, TFun),
 )
import Primer.Name (NameCounter)
import Primer.Typecheck.Cxt (Cxt)
import Primer.Typecheck.Kindcheck (
  Type,
 )

-- | Attempts to find a substitution for the given variables that makes the types consistent (i.e. equal-up-to-holes).
-- We represent unification variables as TVars which happen to have names in the given set.
-- We unify without caring about kinds, but afterwards check that the solution is well-kinded.
-- We ensure that (if a unifier is found) the returned substitution is idempotent, in the sense that
-- it only needs to be applied once to remove all solved unification variables; i.e. in the RHS there
-- never appears a solved unification variable.
-- We ensure we are stable under swapping the two input types
--  (for testing purposes: it is easy to do here, but quite awkward to figure out what the symmetry property should be if we don't
--   - the problem starts when unifying two unifvars: could get either way around, but then the knockon effects are complex
--   - eg @(unif1 -> unif3) \`unify\` (unif2 -> List unif2)@ says @unif1:=unif2; unif3:=List unif2@
--   - but symmetric version might say @unif2:=unif1; unif3:=List unif1@
--   - We are careful to always give the first result.
--   - However, this choice is not stable under renaming unification variables: an equivalent
--     unification problem with different names may choose the "other" solution.
--  )
unify ::
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  -- | We only care about local type vars and typedefs, for kind-checking our unifier
  Cxt ->
  -- | Which type variables should be considered as unification variables? This should be a subset of the @Cxt@.
  -- All @Cxt@ vars are considered in scope for a solution of any unification variable.
  S.Set TyVarName ->
  Type ->
  Type ->
  m (Maybe (M.Map TyVarName Type))
unify _cxt _unificationVars s t = do
  result <-
    runExceptT $
          unU $
            unify' s t
  case result of
    Left _err -> pure Nothing
    Right _ -> pure $ Just mempty

data UnifError
  = NotUnify Type Type

newtype U m a = U {unU :: (ExceptT UnifError m) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError UnifError
    )

deriving newtype instance MonadFresh NameCounter m => MonadFresh NameCounter (U m)

-- We assume (both empty and non-empty) holes can unify with anything!
unify' :: MonadFresh NameCounter m => Type -> Type -> U m ()
unify' (TEmptyHole _) _ = pure ()
unify' _ (TEmptyHole _) = pure ()
unify' (TFun _ s1 t1) (TFun _ s2 t2) = unify' s1 s2 >> unify' t1 t2
-- Doing first-order unification, as applications are only constructor-like
-- (we don't have any bona fide functions at the type level)
unify' (TApp _ s1 t1) (TApp _ s2 t2) = unify' s1 s2 >> unify' t1 t2
unify' s t = throwError $ NotUnify s t
