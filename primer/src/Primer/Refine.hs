module Primer.Refine (refine) where

import Prelude

import Primer.Core.Type (
  Type (TFun),
 )
import Primer.Typecheck (consistentTypes)

refine :: Type -> Type -> Maybe Type
refine tgtTy tmTy = if consistentTypes tgtTy tmTy
          then Just tmTy
          else case tmTy of
                 TFun _ t -> refine tgtTy t
                 _ -> Nothing
