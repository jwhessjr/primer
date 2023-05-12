-- | Tests for the typechecker
module Tests.Typecheck (TypeCacheAlpha (TypeCacheAlpha)) where

import Foreword

import Optics ((%), (%~))
import Primer.App (
  Prog (
    Prog
  ),
  appIdCounter,
  appInit,
  appNameCounter,
  appProg,
 )
import Primer.App qualified as App
import Primer.Core (
  Expr' (..),
  ExprMeta,
  Meta (..),
  TypeCache (..),
  TypeCacheBoth (..),
  _exprMeta,
  _type,
 )
import Primer.Core.Utils (alphaEqTy)
import Primer.Def (
  ASTDef (ASTDef),
  Def (..),
 )
import Primer.Module (Module (..))
import Primer.Name (Name)

-- A helper type for smartholes idempotent tests
-- Equality is as normal, except in the typecache, where it is up-to-alpha
newtype TypeCacheAlpha a = TypeCacheAlpha {unTypeCacheAlpha :: a}
  deriving stock (Show)
instance Eq (TypeCacheAlpha TypeCache) where
  TypeCacheAlpha (TCSynthed s) == TypeCacheAlpha (TCSynthed t) =
    s `alphaEqTy` t
  TypeCacheAlpha (TCChkedAt s) == TypeCacheAlpha (TCChkedAt t) =
    s `alphaEqTy` t
  TypeCacheAlpha (TCEmb (TCBoth s t)) == TypeCacheAlpha (TCEmb (TCBoth s' t')) =
    s `alphaEqTy` s' && t `alphaEqTy` t'
  _ == _ = False
tcaFunctorial :: (Functor f, Eq (f (TypeCacheAlpha a))) => TypeCacheAlpha (f a) -> TypeCacheAlpha (f a) -> Bool
tcaFunctorial = (==) `on` (fmap TypeCacheAlpha . unTypeCacheAlpha)
instance Eq (TypeCacheAlpha a) => Eq (TypeCacheAlpha (Maybe a)) where
  (==) = tcaFunctorial
instance (Eq (TypeCacheAlpha a), Eq b) => Eq (TypeCacheAlpha (Expr' (Meta a) b)) where
  (==) = (==) `on` (((_exprMeta % _type) %~ TypeCacheAlpha) . unTypeCacheAlpha)
instance Eq (TypeCacheAlpha Def) where
  TypeCacheAlpha (DefAST (ASTDef e1 t1)) == TypeCacheAlpha (DefAST (ASTDef e2 t2)) =
    TypeCacheAlpha e1 == TypeCacheAlpha e2 && t1 == t2
  TypeCacheAlpha (DefPrim p1) == TypeCacheAlpha (DefPrim p2) =
    p1 == p2
  _ == _ = False
instance Eq (TypeCacheAlpha (Map Name Def)) where
  (==) = tcaFunctorial
instance Eq (TypeCacheAlpha Module) where
  TypeCacheAlpha (Module n1 tds1 ds1) == TypeCacheAlpha (Module n2 tds2 ds2) =
    n1 == n2 && tds1 == tds2 && TypeCacheAlpha ds1 == TypeCacheAlpha ds2
instance Eq (TypeCacheAlpha [Module]) where
  (==) = tcaFunctorial
instance Eq (TypeCacheAlpha ExprMeta) where
  (==) = tcaFunctorial
instance Eq (TypeCacheAlpha App.NodeSelection) where
  TypeCacheAlpha (App.NodeSelection t1 m1) == TypeCacheAlpha (App.NodeSelection t2 m2) =
    t1 == t2 && ((==) `on` first TypeCacheAlpha) m1 m2
instance Eq (TypeCacheAlpha App.Selection) where
  TypeCacheAlpha (App.Selection d1 n1) == TypeCacheAlpha (App.Selection d2 n2) =
    d1 == d2 && TypeCacheAlpha n1 == TypeCacheAlpha n2
instance Eq (TypeCacheAlpha Prog) where
  TypeCacheAlpha (Prog i1 m1 s1 sh1 l1 r1) == TypeCacheAlpha (Prog i2 m2 s2 sh2 l2 r2) =
    TypeCacheAlpha i1 == TypeCacheAlpha i2
      && TypeCacheAlpha m1 == TypeCacheAlpha m2
      && TypeCacheAlpha s1 == TypeCacheAlpha s2
      && sh1 == sh2
      && l1 == l2
      && r1 == r2
instance Eq (TypeCacheAlpha App.App) where
  TypeCacheAlpha a1 == TypeCacheAlpha a2 =
    appInit a1 == appInit a2
      && appIdCounter a1 == appIdCounter a2
      && appNameCounter a1 == appNameCounter a2
      && TypeCacheAlpha (appProg a1) == TypeCacheAlpha (appProg a2)
