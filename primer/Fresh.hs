module Fresh (MonadFresh (..)) where

import Foreword

-- | This class gives access to a method @fresh@ which generates a new, unique
--  value of type i.
--  We use it for generating new IDs, hole and variables names.
class Monad m => MonadFresh i m where
  fresh :: m i

instance MonadFresh i m => MonadFresh i (ExceptT e m) where
  fresh = lift fresh

instance MonadFresh i m => MonadFresh i (ReaderT e m) where
  fresh = lift fresh
