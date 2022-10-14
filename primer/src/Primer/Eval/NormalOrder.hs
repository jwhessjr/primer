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
    InlineLetrec,
    RenameBindingsCase,
    RenameBindingsLAM,
    RenameBindingsLam,
    RenameSelfLet,
    RenameSelfLetType
  ),
  RedexType (
    ElideLetInType,
    InlineLetInType,
    RenameForall,
    RenameSelfLetInType, var, letBinding, origBinder
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
-- Annoyingly this is not quite leftmost-outermost wrt our Expr type, as we
-- are using 'let's to encode something similar to explicit substitution, and
-- reduce them by substituting one occurrance at a time, removing the 'let'
-- when there are no more substitutions to be done. Note that the 'let' itself
-- doesn't get "pushed down" in the tree.
-- example:
--   lettype a = Bool -> Bool in (λx.not x : a) True
-- reduces to
--   lettype a = Bool -> Bool in (λx.not x : Bool -> Bool) True
-- and then to
--   (λx.not x : Bool -> Bool) True
-- This can be seen as "leftmost-outermost" if you consider the location of the
-- "expand a" redex to be the 'lettype' rather than the variable occurrance.
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
        , subst = Just (\(LSome l) -> fmap evalAccumT . goSubst l)
        , substTy = Just (\v t -> evalAccumT . goSubstTy v t)
        }
    )
  where
    goSubst :: Local k -> ExprZ -> Dir -> AccumT Cxt Maybe RedexWithContext
    goSubst l ez d = do
      hoistAccum (readerToAccumT $ viewRedex tydefs globals d $ target ez) >>= \case
        -- We should inline such 'v' (note that we will not go under any 'v' binders)
        Just r@(InlineLet w _) | localName l == unLocalName w -> pure $ RExpr ez r
        Just r@(InlineLetrec w _ _) | localName l == unLocalName w -> pure $ RExpr ez r
        -- Elide a let only if it blocks the reduction
        Just r@(ElideLet (LSome w) _) | elemOf _freeVarsLocal (localName w) l -> pure $ RExpr ez r
        -- Rename a binder only if it blocks the reduction
        Just r@(RenameBindingsLam _ w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameBindingsLAM _ w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameBindingsCase _ _ brs _)
          | not $ S.disjoint (setOf _freeVarsLocal l) (setOf (folded % #_CaseBranch % _2 % folded % to bindName % to unLocalName) brs) ->
              pure $ RExpr ez r
        Just r@(RenameSelfLet w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameSelfLetType w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
        -- Switch to an inner let if substituting under it would cause capture
        Nothing
          | Just (LSome l', bz) <- viewLet (d, ez)
          , localName l' /= localName l
          , elemOf _freeVarsLocal (localName l') l ->
              (\(d', b) -> goSubst l' b d') =<< hoistAccum bz
        -- We should not go under 'v' binders, but otherwise substitute in each child
        _ ->
          let substChild (d', c) = do
                guard $ S.notMember (localName l) $ getBoundHere (target ez) (Just $ target c)
                goSubst l c d'
              substTyChild c = case l of
                LLetType v t -> goSubstTy v t c
                _ -> mzero
           in msum @[] $ (substTyChild =<< focusType' ez) : map (substChild <=< hoistAccum) (exprChildren (d, ez))
    goSubstTy :: TyVarName -> Type -> TypeZ -> AccumT Cxt Maybe RedexWithContext
    goSubstTy v t tz =
      let isFreeIn = elemOf (getting _freeVarsTy % _2)
       in do
            hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
              -- We should inline such 'v' (note that we will not go under any 'v' binders)
              Just r@(InlineLetInType {var}) | var == v -> pure $ RType tz r
              -- Elide a let only if it blocks the reduction
              Just r@(ElideLetInType {letBinding = (LLetType w _)}) | w `isFreeIn` t -> pure $ RType tz r
              -- Rename a binder only if it blocks the reduction
              Just r@(RenameSelfLetInType {letBinding = (LLetType w _)}) | w `isFreeIn` t -> pure $ RType tz r
              Just r@(RenameForall {origBinder}) | origBinder `isFreeIn` t -> pure $ RType tz r
              -- We switch to an inner let if substituting under it would cause capture
              Nothing
                | TLet _ w s _ <- target tz
                , [_, bz] <- typeChildren tz
                , v /= w
                , w `isFreeIn` t ->
                    goSubstTy w s =<< hoistAccum bz
              -- We should not go under 'v' binders, but otherwise substitute in each child
              _ ->
                let substChild c = do
                      guard $
                        S.notMember (unLocalName v) $
                          S.map unLocalName $
                            getBoundHereTy (target tz) (Just $ target c)
                      goSubstTy v t c
                 in msum $ map (substChild <=< hoistAccum) (typeChildren tz)

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
singletonCxtLet :: HasID i => i -> LVarName -> Expr -> Cxt
singletonCxtLet i x e = addBinds i [Right $ LSome $ LLet x e] `execAccum`  mempty

singletonCxtLetType :: HasID i => i -> TyVarName -> Type -> Cxt
singletonCxtLetType i x t = addBinds i [Right $ LSome $ LLetType x t] `execAccum`  mempty

singletonCxtLetrec :: HasID i => i -> LVarName -> Expr -> Type -> Cxt
singletonCxtLetrec i x e t = addBinds i [Right $ LSome $  LLetrec x e t] `execAccum`  mempty
