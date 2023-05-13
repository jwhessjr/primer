module Primer.Log (
  ConvertLogMessage (..),
  PureLogT,
  runPureLogT,
  PureLog,
  runPureLog,
  DiscardLogT,
  DiscardLog,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (
  DiscardLoggingT,
  LoggingT,
  MonadLog,
  PureLoggingT,
  logMessage,
  runLoggingT,
  runPureLoggingT,
 )
import Control.Monad.Trans (MonadTrans)

class ConvertLogMessage source target where
  convert :: source -> target

-- | Convenient for discarding logging.
instance ConvertLogMessage a () where
  convert = pure ()

-- | Purely collect log messages in a 'Seq'
newtype PureLogT l m a = PureLogs (LoggingT l (PureLoggingT (Seq l) m) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLog l
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadFresh i
    )

instance MonadTrans (PureLogT l) where
  lift = PureLogs . lift . lift

-- | Purely accumulate log messages in a 'Seq'.
-- Note that this may cause a large amount of memory to be retained if you
-- use this with an action that logs excessively.
-- Note that the logs are accumulated strictly, i.e. each element in the
-- resulting 'Seq' will be in WHNF.
runPureLogT :: Monad m => PureLogT l m a -> m (a, Seq l)
runPureLogT (PureLogs m) = runPureLoggingT $ runLoggingT m $ \l ->
  let !l' = l in logMessage $ pure l'

type PureLog l = PureLogT l Identity

runPureLog :: PureLog l a -> (a, Seq l)
runPureLog = runIdentity . runPureLogT

-- | Discard log messages.
newtype DiscardLogT l m a = DiscardLogs (DiscardLoggingT l m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLog l
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadFresh i
    )

instance MonadTrans (DiscardLogT l) where
  lift = DiscardLogs . lift

type DiscardLog l = DiscardLogT l Identity
