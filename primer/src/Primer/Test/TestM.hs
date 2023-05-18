-- A test monad for generating names and IDs and typechecking
module Primer.Test.TestM (
  TestM,
  evalTestM,
) where

import Foreword

newtype TestM a = TM (Identity a)
  deriving newtype (Functor, Applicative, Monad)

evalTestM :: TestM a -> a
evalTestM (TM x) = runIdentity x
