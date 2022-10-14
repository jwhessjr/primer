{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

module Primer.Eval.NormalOrder (
  RedexWithContext (RExpr, RType),
  findRedex,
) where

import Foreword hiding (hoistAccum)
import Foreword qualified

import Control.Monad.Morph (generalize)
import Control.Monad.Trans.Accum (Accum, AccumT, add, evalAccumT, look, readerToAccumT)
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
  getID,
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
    RenameSelfLetInType
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
  up,
 )

-- We don't really want a zipper here, but a one-hole context, but it is easier
-- to just reuse the zipper machinery and ignore the target of the zipper.
-- The ExprZ/TypeZ is a zipper focussed on a subterm which is a Redex as described by the second argument.
data RedexWithContext
  = RExpr ExprZ Redex
  | RType TypeZ RedexType

viewLet :: ExprZ -> Maybe (SomeLocal, Accum Cxt ExprZ)
viewLet ez = case (target ez, exprChildren ez) of
  (Let _ x e _b, [_, bz]) -> Just (LSome $ LLet x e, bz)
  (Letrec _ x e ty _b, [_, bz]) -> Just (LSome $ LLetrec x e ty, bz)
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (LSome $ LLetType a ty, bz)
  _ -> Nothing

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
findRedex tydefs globals topDir = flip evalAccumT mempty . go . focus
  where
    -- What is the direction from the context?
    -- i.e. are we in the head of an elimination (or inside a hole)?
    focusDir :: ExprZ -> Dir
    focusDir ez = case up ez of
      Nothing -> topDir
      Just z -> case target z of
        App _ f _ | f == target ez -> Syn
        APP _ f _ | f == target ez -> Syn
        Case _ scrut _ | scrut == target ez -> Syn
        Hole _ _ -> Syn
        _ -> Chk
    focusType' :: ExprZ -> AccumT Cxt Maybe TypeZ
    -- Note that nothing in Expr binds a variable which scopes over a type child
    -- so we don't need to 'add' anything
    focusType' = lift . focusType
    hoistAccum :: Accum Cxt a -> AccumT Cxt Maybe a
    hoistAccum = Foreword.hoistAccum generalize
    go :: ExprZ -> AccumT Cxt Maybe RedexWithContext
    go ez = do
      hoistAccum (readerToAccumT $ viewRedex tydefs globals (focusDir ez) (target ez)) >>= \case
        Just r -> pure $ RExpr ez r
        Nothing
          | Just (LSome l, bz) <- viewLet ez -> goSubst l =<< hoistAccum bz
          -- Since stuck things other than lets are stuck on the first child or
          -- its type annotation, we can handle them all uniformly
          | otherwise ->
              msum $
                (goType =<< focusType' ez)
                  : map (go <=< hoistAccum) (exprChildren ez)
    goType :: TypeZ -> AccumT Cxt Maybe RedexWithContext
    goType tz = do
      hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
        Just r -> pure $ RType tz r
        Nothing
          | TLet _ a t _body <- target tz
          , [_, bz] <- typeChildren tz ->
              goSubstTy a t =<< hoistAccum bz
          | otherwise -> msum $ map (goType <=< hoistAccum) $ typeChildren tz
    goSubst :: Local k -> ExprZ -> AccumT Cxt Maybe RedexWithContext
    goSubst l ez = do
      hoistAccum (readerToAccumT $ viewRedex tydefs globals (focusDir ez) $ target ez) >>= \case
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
          | Just (LSome l', bz) <- viewLet ez
          , localName l' /= localName l
          , elemOf _freeVarsLocal (localName l') l ->
              goSubst l' =<< hoistAccum bz
        -- We should not go under 'v' binders, but otherwise substitute in each child
        _ ->
          let substChild c = do
                guard $ S.notMember (localName l) $ getBoundHere (target ez) (Just $ target c)
                goSubst l c
              substTyChild c = case l of
                LLetType v t -> goSubstTy v t c
                _ -> mzero
           in msum @[] $ (substTyChild =<< focusType' ez) : map (substChild <=< hoistAccum) (exprChildren ez)
    goSubstTy :: TyVarName -> Type -> TypeZ -> AccumT Cxt Maybe RedexWithContext
    goSubstTy v t tz =
      let isFreeIn = elemOf (getting _freeVarsTy % _2)
       in do
            hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
              -- We should inline such 'v' (note that we will not go under any 'v' binders)
              Just r@(InlineLetInType w _) | w == v -> pure $ RType tz r
              -- Elide a let only if it blocks the reduction
              Just r@(ElideLetInType (LLetType w _) _) | w `isFreeIn` t -> pure $ RType tz r
              -- Rename a binder only if it blocks the reduction
              Just r@(RenameSelfLetInType w _ _) | w `isFreeIn` t -> pure $ RType tz r
              Just r@(RenameForall _ w _ _ _) | w `isFreeIn` t -> pure $ RType tz r
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

exprChildren :: ExprZ -> [Accum Cxt ExprZ]
exprChildren ez =
  children' ez <&> \c -> do
    let bs = getBoundHere' (target ez) (Just $ target c)
    addBinds ez bs
    pure c

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
