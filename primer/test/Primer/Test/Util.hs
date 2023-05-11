-- | Utilities useful across several types of tests.
module Primer.Test.Util (
  testNoSevereLogs,
) where

import Foreword

import Control.Monad.Log (Severity (Informational), WithSeverity (msgSeverity))
import Data.Sequence qualified as Seq
import Hedgehog (MonadTest, (===))
import Primer.Log (ConvertLogMessage (convert),)

newtype LogMsg = LogMsg Text
  deriving newtype (Show)

instance Show l => ConvertLogMessage l LogMsg where
  convert = LogMsg . show

isSevereLog :: WithSeverity l -> Bool
isSevereLog l = msgSeverity l < Informational

testNoSevereLogs :: (HasCallStack, MonadTest m, Eq l, Show l) => Seq (WithSeverity l) -> m ()
testNoSevereLogs logs = Seq.filter isSevereLog logs === mempty
