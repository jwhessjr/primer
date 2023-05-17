module Primer.Unification (InternalUnifyError (..), unify) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as M
import Data.Set qualified as S
import Optics (anyOf, getting, over, set)
import Primer.Core.Meta (
  ID,
  TyVarName,
  trivialMeta,
 )
import Primer.Core.Type (
  Type' (TApp, TEmptyHole, TFun),
  _typeMeta,
 )
import Primer.Core.Type.Utils (_freeVarsTy)
import Primer.Name (NameCounter)
import Primer.Subst (substTy)
import Primer.Typecheck.Cxt (Cxt)
import Primer.Typecheck.Kindcheck (
  KindError,
  Type,
  checkKind,
  consistentKinds,
  lookupLocalTy,
 )

-- | This should never be thrown - it indicates a bug in either this module, or in how it is called
data InternalUnifyError
  = InternalUnifyVarNotInCxt Cxt TyVarName
  deriving stock (Show)

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
  (MonadFresh ID m, MonadFresh NameCounter m, MonadError InternalUnifyError m) =>
  -- | We only care about local type vars and typedefs, for kind-checking our unifier
  Cxt ->
  -- | Which type variables should be considered as unification variables? This should be a subset of the @Cxt@.
  -- All @Cxt@ vars are considered in scope for a solution of any unification variable.
  S.Set TyVarName ->
  Type ->
  Type ->
  m (Maybe (M.Map TyVarName Type))
unify cxt unificationVars s t = do
  result <-
    runExceptT $
      flip execStateT mempty $
        flip runReaderT initEnv $
          unU $
            unify' s t
  case result of
    Left _err -> pure Nothing
    Right sb -> do
      -- We need Type' (Meta a) to call checkKind, but we know the meta will
      -- not be inspected in any way we care about, since we only care whether
      -- checkKind succeeded, and not the result. Thus we just add some dummy
      -- ones.
      -- TODO: this is a bit of a code smell!
      let addPointlessMeta = set _typeMeta $ trivialMeta 0
      let f v vt = case lookupLocalTy v cxt of
            Right k -> All . isRight <$> runExceptT @KindError (runReaderT (checkKind k $ addPointlessMeta vt) cxt)
            -- this catchall should never happen: sb should only contain
            -- solutions for unification variables, which should be a subset
            -- of the context!
            _ -> Ap $ throwError $ InternalUnifyVarNotInCxt cxt v
      goodKinds <- getAll <$> getAp (M.foldMapWithKey f sb)
      pure $ if goodKinds then Just sb else Nothing
  where
    initEnv = Env{unifVars = unificationVars, boundVarsL = mempty, boundVarsR = mempty}

type UnifVars = S.Set TyVarName

data UnifError
  = NotUnify Type Type
  | OccursBoundCheckFail TyVarName Type

-- NB: we need to keep the input types on the same side always, to get boundVars info to line up
-- or rather, ensure we record the swap if we do a swap
data Env = Env
  { unifVars :: UnifVars
  -- ^ The variables originally declared to be uvs.
  -- These may be shadowed by bound variables (this set is not updated in that case).
  , boundVarsL :: M.Map TyVarName Int
  -- ^ Variables bound in the LHS of the unification problem.
  -- i.e. @forall@s that we have gone under, with their de Bruijn level
  , boundVarsR :: M.Map TyVarName Int
  -- ^ as 'boundVarsL', but for the RHS
  }

type Subst = M.Map TyVarName Type

newtype U m a = U {unU :: ReaderT Env (StateT Subst (ExceptT UnifError m)) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadState Subst
    , MonadError UnifError
    )

deriving newtype instance MonadFresh NameCounter m => MonadFresh NameCounter (U m)

-- | @v@ is a unification variable if it both
-- - was declared to be, and
-- - has not been shadowed (by going under a equally-named forall)
isUnifVarL, isUnifVarR :: MonadFresh NameCounter m => TyVarName -> U m Bool
isUnifVarL n = asks (\env -> S.member n (unifVars env) && not (M.member n $ boundVarsL env))
isUnifVarR n = asks (\env -> S.member n (unifVars env) && not (M.member n $ boundVarsR env))

isSameVar :: MonadFresh NameCounter m => TyVarName -> TyVarName -> U m Bool
isSameVar n m = do
  nIdx <- asks (M.lookup n . boundVarsL)
  mIdx <- asks (M.lookup m . boundVarsR)
  pure $ case (nIdx, mIdx) of
    (Just i, Just j) -> i == j -- both locally bound (possibly with different names in the left/right type)
    (Nothing, Nothing) -> n == m -- both from the global context
    _ -> False -- locally-bound (forall) vars never unify with a variable from the context

swapEnv :: Env -> Env
swapEnv e = e{boundVarsL = boundVarsR e, boundVarsR = boundVarsL e}

-- Note: bound variables shadow unification variables.
-- This is handled in isUnifVarL and isUnifVarR
bind :: TyVarName -> TyVarName -> Env -> Env
bind n m e =
  e
    { boundVarsL = M.insert n (M.size $ boundVarsL e) $ boundVarsL e
    , boundVarsR = M.insert m (M.size $ boundVarsR e) $ boundVarsR e
    }

lookupSubst :: MonadFresh NameCounter m => TyVarName -> U m (Maybe Type)
lookupSubst = gets . M.lookup

-- We assume (both empty and non-empty) holes can unify with anything!
unify' :: MonadFresh NameCounter m => Type -> Type -> U m ()
unify' (TEmptyHole _) _ = pure ()
unify' _ (TEmptyHole _) = pure ()
unify' (TFun _ s1 t1) (TFun _ s2 t2) = unify' s1 s2 >> unify' t1 t2
-- Doing first-order unification, as applications are only constructor-like
-- (we don't have any bona fide functions at the type level)
unify' (TApp _ s1 t1) (TApp _ s2 t2) = unify' s1 s2 >> unify' t1 t2
unify' s t = throwError $ NotUnify s t
