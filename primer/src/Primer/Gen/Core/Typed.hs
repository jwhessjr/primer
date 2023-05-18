{-# LANGUAGE ApplicativeDo #-}

module Primer.Gen.Core.Typed (
  WT,
  genWTType,
  genWTKind,
  forAllT,
  propertyWT,
  synthTest,
) where

import Foreword hiding (mod)

import Control.Monad.Morph (hoist)
import Hedgehog (
  Property, property,
  GenT,
  PropertyT, annotateShow, failure,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Primer.Core (
  Expr,
 )
import Primer.Core.Type (
  Kind (..),
  Type (..)
 )
import Primer.Typecheck (
  TypeError, synth
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

newtype WT a = WT {unWT :: Identity a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    )

-- | Generates types which infer kinds consistent with the argument
-- I.e. @genWTType k@ will generate types @ty@ such that @synthKind ty = k'@
-- with @consistentKinds k k'@. See 'Tests.Gen.Core.Typed.tasty_genTy'
genWTType :: Monad m => Kind -> GenT m Type
genWTType k = do
  let rec = app : catMaybes [arrow]
  Gen.recursive Gen.choice [ehole] rec
  where
    ehole = pure $ TEmptyHole
    app = do k' <- genWTKind; TApp <$> genWTType (KFun k' k) <*> genWTType k'
    arrow =
      if k == KHole || k == KType
        then Just $ TFun <$> genWTType KType <*> genWTType KType
        else Nothing

-- | Generates an arbitary kind. Note that all kinds are well-formed.
genWTKind :: Monad m => GenT m Kind
genWTKind = Gen.recursive Gen.choice [pure KType] [KFun <$> genWTKind <*> genWTKind]

hoist' :: Applicative f => WT a -> f a
hoist' = pure . runIdentity . unWT

-- | Convert a @PropertyT WT ()@ into a @Property@, which Hedgehog can test.
-- It is recommended to do more than default number of tests when using this module.
-- That is to say, generating well-typed syntax is hard, and you probably want
-- to increase the number of tests run to get decent coverage.
-- The modules form the 'Cxt' in the environment of the 'WT' monad
-- (thus the definitions of terms is ignored)
propertyWT :: PropertyT WT () -> Property
propertyWT = property . hoist hoist'

-- Lift 'synth' into a property
synthTest :: HasCallStack => Expr -> PropertyT WT (Type, Expr)
synthTest e = do
  x <- lift $ runExceptT @TypeError $ synth e
  case x of
    Left err -> withFrozenCallStack $ annotateShow err >> failure
    Right y -> pure y
