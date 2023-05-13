module Primer.Gen.Core.Typed (
  WT,
  isolateWT,
  forAllT,
  propertyWT,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh, fresh)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (mapReaderT)
import Hedgehog (
  Property, property,
  GenT,
  PropertyT,
 )
import Hedgehog.Internal.Property (forAllT)
import Primer.Core (
  ID (),
 )
import Primer.Core.DSL (S)
import Primer.Module (Module (..))
import Primer.Name (NameCounter)
import Primer.Test.TestM (
  TestM,
  evalTestM,
  isolateTestM,
 )
import Primer.Typecheck (
  Cxt (),
  SmartHoles (NoSmartHoles),
  buildTypingContextFromModules',
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



newtype WT a = WT {unWT :: ReaderT Cxt TestM a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Cxt
    , MonadFresh NameCounter
    , MonadFresh ID
    )

-- | Run an action and ignore any effect on the fresh name/id state
isolateWT :: WT a -> WT a
isolateWT x = WT $ mapReaderT isolateTestM $ unWT x

instance MonadFresh NameCounter (GenT WT) where
  fresh = lift fresh

instance MonadFresh ID (GenT WT) where
  fresh = lift fresh

instance MonadFresh NameCounter (PropertyT WT) where
  fresh = lift fresh

instance MonadFresh ID (PropertyT WT) where
  fresh = lift fresh

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
