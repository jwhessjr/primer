module Foreword (
  module Protolude,
  module Unsafe,
  module Catch,
  module Foldable,
  modifyError,
  mwhen,
) where

-- In general, we should defer to "Protolude"'s exports and avoid name
-- clashes, but if there's a name that we really want to use and it's
-- unlikely we'll need the "Protolude" version much, we hide the
-- "Protolude" export. When we do hide a "Protolude" export, we try to
-- hide related functions & types, as well, under the assumption that
-- if you ever do need it, you can just import the original module
-- qualified. Examples are "Control.Monad.STM", 'GHC.Generics.from',
-- and 'GHC.Generics.to'.
import Protolude hiding (
  Handler,
  Meta,
  OnDecodeError,
  OnError,
  STM,
  Type,
  TypeError,
  TypeRep,
  UnicodeException,
  atomically,
  bracket,
  bracketOnError,
  bracket_,
  cast,
  catch,
  catchJust,
  catchSTM,
  catches,
  check,
  eqT,
  finally,
  -- hide foldMap as it is lazy in the accumulator
  foldMap,
  -- hide foldl as it is lazy in the accumulator
  foldl,
  from,
  gcast,
  handle,
  handleJust,
  ignore,
  lenientDecode,
  mask,
  mask_,
  maximum,
  moduleName,
  onException,
  orElse,
  replace,
  retry,
  strictDecode,
  throwSTM,
  to,
  try,
  tryJust,
  typeOf,
  typeRep,
  uninterruptibleMask,
  uninterruptibleMask_,
  (%),
 )

-- We should remove all uses of `unsafeHead`. See:
-- https://github.com/hackworthltd/primer/issues/147

import Protolude.Unsafe as Unsafe (unsafeHead)

import Data.Foldable as Foldable (foldMap')

-- We want @exceptions@ rather than @base@'s equivalents.
import Control.Monad.Catch as Catch

-- | Change the type of an error.
modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f = runExceptT >=> either (throwError . f) pure

-- | @mwhen b x@ is `x` if `b` is 'True', otherwise it is 'mempty'.
-- It's like 'Control.Monad.when' but for Monoids rather than Applicatives.
mwhen :: Monoid a => Bool -> a -> a
mwhen b x = if b then x else mempty
