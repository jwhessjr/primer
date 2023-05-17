module Primer.Typecheck.Utils (
  typeOf,
  _typecache,
) where


import Optics (Lens', view, (%))
import Primer.Core (Expr', TypeCache, _exprMetaLens)
import Primer.Core.Meta (Meta, _type)
import Primer.Core.Type (Kind)


-- | A lens for the type annotation of an 'Expr' or 'ExprT'
_typecache :: Lens' (Expr' (Meta a) b) a
_typecache = _exprMetaLens % _type

-- | Get the type of an 'ExprT'
typeOf :: Expr' (Meta TypeCache) (Meta Kind) -> TypeCache
typeOf = view _typecache
