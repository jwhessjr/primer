module Foreword (
  module Protolude,
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
