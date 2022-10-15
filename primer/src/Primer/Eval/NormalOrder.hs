{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Primer.Eval.NormalOrder (
  RedexWithContext (RExpr, RType),
  findRedex,
  foldMapExpr,
  FMExpr(..),
  -- Exported for testing
  singletonCxtLet,
  singletonCxtLetType,
  singletonCxtLetrec,
) where

import Foreword hiding (hoistAccum)
import Foreword qualified

import Control.Monad.Morph (generalize)
import Control.Monad.Trans.Accum (
  Accum,
  AccumT,
  add,
  evalAccumT,
  look,
  readerToAccumT, execAccum,
 )
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Optics (
  elemOf,
  folded,
  getting,
  to,
  (%),
  _2,
 )
import Primer.Core (
  Expr,
  Expr' (
    APP,
    App,
    Case,
    Hole,
    Let,
    LetType,
    Letrec
  ),
  HasID,
  LocalName (unLocalName),
  LocalNameKind (ATyVar),
  TyVarName,
  Type,
  Type' (
    TLet
  ),
  bindName,
  HasID, getID, LVarName,
 )
import Primer.Core.Utils (
  _freeVarsTy,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Eval.Redex (
  Cxt (Cxt),
  Dir (Chk, Syn),
  Local (LLet, LLetType, LLetrec),
  Redex (
    ElideLet,
    InlineLet,
    RenameBindingsCase,
    RenameBindingsLAM,
    RenameBindingsLam
  ),
  RedexType (
    ElideLetInType,
    InlineLetInType,
    RenameForall, var, letBinding, origBinder
  ),
  SomeLocal (LSome),
  localName,
  viewRedex,
  viewRedexType,
  _freeVarsLocal,
 )
import Primer.Name (Name)
import Primer.TypeDef (
  TypeDefMap,
 )
import Primer.Zipper (
  ExprZ,
  IsZipper,
  TypeZ,
  down,
  focus,
  focusType,
  getBoundHere,
  getBoundHereTy,
  right,
  target,
 )

-- We don't really want a zipper here, but a one-hole context, but it is easier
-- to just reuse the zipper machinery and ignore the target of the zipper.
-- The ExprZ/TypeZ is a zipper focussed on a subterm which is a Redex as described by the second argument.
data RedexWithContext
  = RExpr ExprZ Redex
  | RType TypeZ RedexType

viewLet :: (Dir, ExprZ) -> Maybe (SomeLocal, Accum Cxt (Dir, ExprZ))
viewLet dez@(_, ez) = case (target ez, exprChildren dez) of
  (Let _ x e _b, [_, bz]) -> Just (LSome $ LLet x e, bz)
  (Letrec _ x e ty _b, [_, bz]) -> Just (LSome $ LLetrec x e ty, bz)
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (LSome $ LLetType a ty, bz)
  _ -> Nothing

-- TODO: better docs
-- not quite a normal fold map:
-- - handle context & bidirectionality
-- - go over every Expr node, but also every Type node
-- - special handling for lets, because we need that for normal order
-- goes in normal order modulo lets
foldMapExpr :: forall f a. MonadPlus f => FMExpr (f a) -> Dir -> Expr -> f a
foldMapExpr extract topDir = flip evalAccumT mempty . go . (topDir,) . focus
  where
    go :: (Dir, ExprZ) -> AccumT Cxt f a
    go dez@(d, ez) =
      readerToAccumT (ReaderT $ extract.expr ez d)
        <|> case (extract.subst, viewLet dez) of
          (Just goSubst, Just (l, bz)) -> (readerToAccumT . ReaderT . (\(d', b) -> goSubst l b d')) =<< hoistAccum bz
          -- Since stuck things other than lets are stuck on the first child or
          -- its type annotation, we can handle them all uniformly
          _ ->
            msum $
              (goType =<< focusType' ez)
                : map (go <=< hoistAccum) (exprChildren dez)
    goType :: TypeZ -> AccumT Cxt f a
    goType tz =
      readerToAccumT (ReaderT $ extract.ty tz)
        <|> case (extract.substTy, target tz) of
          (Just goSubstTy, TLet _ a t _body)
            | [_, bz] <- typeChildren tz -> (readerToAccumT . ReaderT . goSubstTy a t) =<< hoistAccum bz
          _ -> msum $ map (goType <=< hoistAccum) $ typeChildren tz

data FMExpr m = FMExpr
  { expr :: ExprZ -> Dir -> Cxt -> m
  , ty :: TypeZ -> Cxt -> m
  , subst :: Maybe (SomeLocal -> ExprZ {- The body of the let-} -> Dir -> Cxt -> m)
  , substTy :: Maybe (TyVarName -> Type -> TypeZ -> Cxt -> m)
  }

focusType' :: MonadPlus m => ExprZ -> AccumT Cxt m TypeZ
-- Note that nothing in Expr binds a variable which scopes over a type child
-- so we don't need to 'add' anything
focusType' = lift . maybe empty pure . focusType

hoistAccum :: Monad m => Accum Cxt b -> AccumT Cxt m b
hoistAccum = Foreword.hoistAccum generalize

-- We find the normal-order redex.
findRedex ::
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  Maybe RedexWithContext
findRedex tydefs globals =
  foldMapExpr
    ( FMExpr
        { expr = \ez d -> runReader (RExpr ez <<$>> viewRedex tydefs globals d (target ez))
        , ty = \tz -> runReader (RType tz <<$>> viewRedexType (target tz))
        , subst = Nothing
        , substTy = Nothing
        }
    )

children' :: IsZipper za a => za -> [za]
children' z = case down z of
  Nothing -> mempty
  Just z' -> z' : unfoldr (fmap (\x -> (x, x)) . right) z'

exprChildren :: (Dir, ExprZ) -> [Accum Cxt (Dir, ExprZ)]
exprChildren (d, ez) =
  children' ez <&> \c -> do
    let bs = getBoundHere' (target ez) (Just $ target c)
    let d' = case target ez of
          App _ f _ | f == target c -> Syn
          APP _ f _ | f == target c -> Syn
          Case _ scrut _ | scrut == target c -> Syn
          Hole _ _ -> Syn
          -- bodies of lets are the same direction as
          -- the let themselves
          Let _ _ e _ | e == target c -> Syn
                      | otherwise -> d
          LetType{} -> d
          Letrec _ _ e _ _ | e == target c -> Chk
                           | otherwise -> d
          _ -> Chk
    addBinds ez bs
    pure (d', c)

typeChildren :: TypeZ -> [Accum Cxt TypeZ]
typeChildren tz =
  children' tz <&> \c -> do
    let bs = getBoundHereTy' (target tz) (Just $ target c)
    addBinds tz bs
    pure c

-- TODO: Yuck, is there another way?
getBoundHere' :: Expr -> Maybe Expr -> [Either Name SomeLocal]
getBoundHere' (Let _ x e1 e2) boundIn | boundIn == Just e2 = [Right $ LSome $ LLet x e1]
getBoundHere' (LetType _ a t _) _ = [Right $ LSome $ LLetType a t]
getBoundHere' (Letrec _ x e1 t _) _ = [Right $ LSome $ LLetrec x e1 t]
getBoundHere' boundAt boundIn = map Left $ S.toList $ getBoundHere boundAt boundIn

getBoundHereTy' :: Type -> Maybe Type -> [Either Name SomeLocal]
getBoundHereTy' = fmap (bimap unLocalName LSome) <<$>> getBoundHereTy''
  where
    getBoundHereTy'' :: Type -> Maybe Type -> [Either TyVarName (Local 'ATyVar)]
    getBoundHereTy'' (TLet _ x t1 t2) boundIn | boundIn == Just t2 = [Right $ LLetType x t1]
    getBoundHereTy'' boundAt boundIn = map Left $ S.toList $ getBoundHereTy boundAt boundIn

addBinds :: HasID i => i -> [Either Name SomeLocal] -> Accum Cxt ()
addBinds i' bs = do
  let i = getID i'
  cxt <- look
  add $
    Cxt $
      M.fromList $
        bs <&> \case
          Left n -> (n, (Nothing, i, cxt))
          Right ls@(LSome l) -> (localName l, (Just ls, i, cxt))

-- TODO: cannot easily export Local as ctors conflict with old Locals
-- so difficult to join these three into a singletonCxt to replace singletonLocal
-- TODO: now have finished details work, we can join these up
singletonCxtLet :: HasID i => i -> LVarName -> Expr -> Cxt
singletonCxtLet i x e = addBinds i [Right $ LSome $ LLet x e] `execAccum`  mempty

singletonCxtLetType :: HasID i => i -> TyVarName -> Type -> Cxt
singletonCxtLetType i x t = addBinds i [Right $ LSome $ LLetType x t] `execAccum`  mempty

singletonCxtLetrec :: HasID i => i -> LVarName -> Expr -> Type -> Cxt
singletonCxtLetrec i x e t = addBinds i [Right $ LSome $  LLetrec x e t] `execAccum`  mempty
