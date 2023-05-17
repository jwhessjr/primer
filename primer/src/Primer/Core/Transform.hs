module Primer.Core.Transform (
  unfoldTApp,
  decomposeTAppCon,
  foldTApp,
  mkTAppCon,
) where

import Foreword

import Primer.Core (
  Type' (..),
 )
import Primer.Core.Meta (TyConName)

-- | Unfold a nested type-level application into the application head and a list of arguments.
unfoldTApp :: Type' a -> (Type' a, [Type' a])
unfoldTApp = second reverse . go
  where
    go (TApp _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Fold an type-level application head and a list of arguments into a single expression.
foldTApp :: (Monad m, Foldable t) => m a -> Type' a -> t (Type' a) -> m (Type' a)
foldTApp m = foldlM $ \a b -> (\m' -> TApp m' a b) <$> m

-- | @mkTAppCon C [X,Y,Z] = C X Y Z@
mkTAppCon :: TyConName -> [Type' ()] -> Type' ()
mkTAppCon c = runIdentity . foldTApp (pure ()) (TCon () c)

-- | Decompose @C X Y Z@ to @(C,[X,Y,Z])@
decomposeTAppCon :: Type' a -> Maybe (TyConName, [Type' a])
decomposeTAppCon = const Nothing
