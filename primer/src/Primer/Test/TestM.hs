-- A test monad for generating names and IDs and typechecking
module Primer.Test.TestM (
  TestM,
  evalTestM,
) where

import Foreword

import Control.Monad.Fresh
import Primer.Core (ID (..))

-- This monad is responsible for generating fresh IDs and names in tests.
-- If we need other abilities, this will be the base monad.
newtype TestM a = TestM {unTestM :: State Int a}
  deriving newtype (Functor, Applicative, Monad)

evalTestM :: ID -> TestM a -> a
evalTestM (ID id_) = fst . flip runState id_ . unTestM

instance MonadFresh ID TestM where
  fresh = TestM $ do
    i <- get
    put $ i + 1
    pure $ ID i
