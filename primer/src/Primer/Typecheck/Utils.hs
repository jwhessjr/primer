module Primer.Typecheck.Utils (
  _typecache,
) where


import Optics (Lens', (%))
import Primer.Core (Expr', _exprMetaLens)
import Primer.Core.Meta (Meta, _type)

-- | A lens for the type annotation of an 'Expr' or 'ExprT'
_typecache :: Lens' (Expr' (Meta a) b) a
_typecache = _exprMetaLens % _type
