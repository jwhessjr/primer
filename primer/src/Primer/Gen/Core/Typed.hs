{-# LANGUAGE ApplicativeDo #-}

module Primer.Gen.Core.Typed (
  WT,
  genWTType,
  genWTKind,
  genSyns,
  genChk,
  genInstApp,
  forAllT,
  propertyWT,
  synthTest,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh, fresh)
import Control.Monad.Morph (hoist)
import Data.Map qualified as M
import Hedgehog (
  Property, property,
  GenT,
  PropertyT, annotateShow, failure,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Primer.Core (
  CaseBranch' (CaseBranch),
  Expr' (..),
  ID (),
  Kind (..),
  TyVarName,
  Type' (..), Expr, unsafeMkGlobalName,
 )
import Primer.Core.DSL (S)
import Primer.Gen.Core.Raw (genTyVarName)
import Primer.Module (Module (..))
import Primer.Name (NameCounter)
import Primer.Refine (Inst (InstAPP, InstApp, InstUnconstrainedAPP))
import Primer.Subst (substTySimul)
import Primer.Test.TestM (
  TestM,
  evalTestM,
 )
import Primer.TypeDef (
  typeDefKind,
 )
import Primer.Typecheck (
  Cxt (),
  SmartHoles (NoSmartHoles),
  buildTypingContextFromModules',
  consistentKinds,
  extendLocalCxtTy,
  localTyVars,
  typeDefs, ExprT, TypeError, synth,
 )

{-
Generate well scoped and typed expressions.
We run in a GenT WT monad, so we have a Reader Cxt and a TestM in our monad
stack when running generators. We are using the TC's Cxt to keep track of what
is in scope, but ignore the smartholes-ness. The TestM satisfies MonadFresh
constraints, which are only used for generating fresh names. Since it is too
awkward to generate correct TypeCache information, and IDs are not needed
(other than global variables) except for communication with the frontend, we
generate un-adorned types and expressions. Unfortunately, some bits of the
backend (especially the typechecker) work in terms of annotated
types/expressions, but it is easy to have a post-processing step of adding IDs
and empty TypeCaches to everything.
-}

type TypeG = Type' ()

type ExprG = Expr' () ()

newtype WT a = WT {unWT :: ReaderT Cxt TestM a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Cxt
    , MonadFresh NameCounter
    , MonadFresh ID
    )

instance MonadFresh NameCounter (GenT WT) where
  fresh = lift fresh

instance MonadFresh ID (GenT WT) where
  fresh = lift fresh

instance MonadFresh NameCounter (PropertyT WT) where
  fresh = lift fresh

instance MonadFresh ID (PropertyT WT) where
  fresh = lift fresh

-- genSyns T with cxt Γ should generate (e,S) st Γ |- e ∈ S and S ~ T (i.e. same up to holes and alpha)
genSyns :: TypeG -> GenT WT (ExprG, TypeG)
genSyns ty = do
  Gen.choice [genEmptyHole, genAnn]
  where
    genEmptyHole = pure (EmptyHole (), TEmptyHole ())
    genAnn = do
      t <- genChk ty
      pure (Ann () t ty, ty)

-- | Given an output of 'refine', e.g. @refine cxt tgtTy srcTy = Just (insts, resTy)@,
-- generate some concrete types and terms corresponding to the instantiation.
-- If @genInstApp insts = (sub, apps)@, then:
-- - @apps@ is the same length as @insts@, and the entries correspond in the way
--   documented by 'refine'.
-- - the size of @sub@ is the number of 'InstUnconstrainedApp' in @inst@, and
--   these entries correspond (by name).
-- - thus if @insts !! n = InstUnconstrainedAPP a k@, then (for some type @t@ of kind @k@)
--   @sub ! a = t@ and @apps !! n = Left t@.
-- - @sub@ is idempotent, and @apps@ do not refer to these names. I.e. the names
--   in @InstUnconstrainedAPP@ do not appear free in @apps@ or the rhs of @sub@.
genInstApp :: [Inst] -> GenT WT (Map TyVarName (Type' ()), [Either TypeG ExprG])
genInstApp = reify mempty
  where
    reify sb = \case
      [] -> pure (sb, [])
      InstApp t : is -> (\a -> second (Right a :)) <$> (substTySimul sb t >>= genChk) <*> reify sb is
      InstAPP t : is -> (\t' -> second (Left t' :)) <$> substTySimul sb t <*> reify sb is
      InstUnconstrainedAPP v k : is -> genWTType k >>= \t' -> second (Left t' :) <$> reify (M.insert v t' sb) is

genChk :: TypeG -> GenT WT ExprG
genChk ty = do
  cse <- lift case_
  let rec = emb : catMaybes [cse]
  Gen.recursive Gen.choice [emb] rec
  where
    emb = fst <$> genSyns ty
    case_ :: WT (Maybe (GenT WT ExprG))
    case_ = pure $ Just $ pure $ Case () (EmptyHole ()) [CaseBranch (unsafeMkGlobalName (pure "M","C")) [] $ EmptyHole ()]

-- | Generates types which infer kinds consistent with the argument
-- I.e. @genWTType k@ will generate types @ty@ such that @synthKind ty = k'@
-- with @consistentKinds k k'@. See 'Tests.Gen.Core.Typed.tasty_genTy'
genWTType :: Kind -> GenT WT TypeG
genWTType k = do
  vars <- lift vari
  cons <- lift constr
  let nonrec = ehole : catMaybes [vars, cons]
  let rec = hole : app : catMaybes [arrow, poly]
  Gen.recursive Gen.choice nonrec rec
  where
    ehole :: GenT WT TypeG
    ehole = pure $ TEmptyHole ()
    hole :: GenT WT TypeG
    hole = THole () <$> genWTType KHole
    app = do k' <- genWTKind; TApp () <$> genWTType (KFun k' k) <*> genWTType k'
    vari :: WT (Maybe (GenT WT TypeG))
    vari = do
      goodVars <- filter (consistentKinds k . snd) . M.toList <$> asks localTyVars
      pure $
        if null goodVars
          then Nothing
          else Just $ Gen.element $ map (TVar () . fst) goodVars
    constr :: WT (Maybe (GenT WT TypeG))
    constr = do
      tds <- asks $ M.assocs . typeDefs
      let goodTCons = filter (consistentKinds k . typeDefKind . snd) tds
      pure $
        if null goodTCons
          then Nothing
          else Just $ Gen.element $ map (TCon () . fst) goodTCons
    arrow :: Maybe (GenT WT TypeG)
    arrow =
      if k == KHole || k == KType
        then Just $ TFun () <$> genWTType KType <*> genWTType KType
        else Nothing
    {- TODO: reinstate once the TC handles them! and then be careful to do
               interesting things where we need to expand the synonym
               See https://github.com/hackworthltd/primer/issues/5
    tlet :: GenT WT TypeG
    tlet = do
      k' <- genWTKind
      n <- genTyVarName
      TLet () n <$> genWTType k' <*> local (extendLocalCxtTy (n,k')) (genWTType k)
    -}
    poly :: Maybe (GenT WT TypeG)
    poly =
      if k == KHole || k == KType
        then Just $ do
          k' <- genWTKind
          n <- genTyVarName
          TForall () n k' <$> local (extendLocalCxtTy (n, k')) (genWTType KType)
        else Nothing

-- | Generates an arbitary kind. Note that all kinds are well-formed.
genWTKind :: GenT WT Kind
genWTKind = Gen.recursive Gen.choice [pure KType] [KFun <$> genWTKind <*> genWTKind]

hoist' :: Applicative f => Cxt -> WT a -> f a
hoist' cxt = pure . evalTestM 0 . flip runReaderT cxt . unWT

-- | Convert a @PropertyT WT ()@ into a @Property@, which Hedgehog can test.
-- It is recommended to do more than default number of tests when using this module.
-- That is to say, generating well-typed syntax is hard, and you probably want
-- to increase the number of tests run to get decent coverage.
-- The modules form the 'Cxt' in the environment of the 'WT' monad
-- (thus the definitions of terms is ignored)
propertyWT :: [S Module] -> PropertyT WT () -> Property
propertyWT mods = property . hoist (hoist' $ buildTypingContextFromModules' mods NoSmartHoles)

-- Lift 'synth' into a property
synthTest :: HasCallStack => Expr -> PropertyT WT (Type' (), ExprT)
synthTest e = do
  x <- lift $ runExceptT @TypeError $ synth e
  case x of
    Left err -> withFrozenCallStack $ annotateShow err >> failure
    Right y -> pure y
