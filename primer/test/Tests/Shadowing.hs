module Tests.Shadowing where

import Foreword

import Hedgehog hiding (Property, check, property, withDiscards, withTests)
import Data.Data (Data)
import Primer.Core
import Primer.Name
import qualified Data.Tree as T
import qualified Data.Set as Set
import qualified Data.Generics.Uniplate.Data as U
import Primer.Zipper
import Optics
import Data.Tree.Optics (root)
import TestUtils
import Primer.Builtins
import Primer.Primitives
import Gen.Core.Typed
import Primer.Core.Utils
import Tests.EvalFull
import Primer.EvalFull
import Primer.Typecheck
import Primer.Module
import Tests.Gen.Core.Typed (propertyWTInExtendedGlobalCxt)
import Primer.Core.Transform (unfoldApp, unfoldAPP)
import Primer.Core.DSL 
import Test.Tasty.HUnit hiding ((@?=))
import qualified Data.Map as Map
import Gen.Core.Raw (genName, genModuleName)
import Primer.Action.Available (actionsForDefBody, actionsForDef)
import qualified Hedgehog.Gen as Gen
import Data.List.Extra (enumerate)
import Hedgehog.Internal.Property (forAllWithT)
import Primer.Action
import qualified Data.Text as T
import Primer.App (appProg, Prog (..), handleEditRequest, runEditAppM, EditAppM)
import qualified Primer.App as App


-- The 'a' parameter (node labels) are only needed for implementation of 'binderTree'
data Tree a b = Node a [(b,Tree a b)]
  deriving Show

instance Bifunctor Tree where
  bimap f g (Node a xs) = Node (f a) $ map (bimap g (bimap f g)) xs

--drawTree :: Tree String String -> String
drawTree = T.drawTree . f
  where
    f (Node a xs) = T.Node a $ map (\(b,t) -> f t & root %~ (("[" <> b <> "]--") <>)) xs


foldTree :: (a -> [(b,c)] -> c) -> Tree a b -> c
foldTree f (Node a xs) = f a $ map (second $ foldTree f) xs

-- NB: there are no children in kinds, so we need not look in the metadata
-- NB: any binder in types (∀ only) scopes over all type children
binderTreeTy :: Data b => Type' b -> Tree () (Set Name)
binderTreeTy = U.para $ \ty children ->
  Node () $ map (Set.map unLocalName $ getBoundHereTy ty,) children

-- Note this DOES NOT check if anything in the metadata's TypeCache is
-- shadowed (or is shadowing) Currently it happens that we can ascribe
-- a type of '∀a. _' to a subterm that happens to be under an 'a'
-- binder. See https://github.com/hackworthltd/primer/issues/556
noShadowing :: (Data a, Data b, Eq a, Eq b) => Expr' a b -> Shadowing
noShadowing = checkShadowing . binderTree

noShadowingTy :: Data b => Type' b -> Shadowing
noShadowingTy = checkShadowing . binderTreeTy

binderTree :: forall a b. (Data a, Data b, Eq a, Eq b) => Expr' a b -> Tree () (Set Name)
binderTree = noNodeLabels' . go
  where
    noNodeLabels :: Tree () b' -> Tree (Maybe a') b'
    noNodeLabels = bimap (const Nothing) identity
    noNodeLabels' :: Tree a' b' -> Tree () b'
    noNodeLabels' = bimap (const ()) identity
    go :: Expr' a b -> Tree (Maybe (Expr' a b)) (Set Name)
    go = U.para $ \e exprChildren' ->
      let exprChildren = map (\c@(Node c' _) ->  (case c' of
                                                    Nothing -> mempty -- no term binders scope over metadata or type children
                                                    Just c'' -> getBoundHereUp $ FA {prior = c'', current = e},c))
                         exprChildren'
          typeChildren = case target . focusOnlyType <$> focusType (focus e) of
            Just ty -> [binderTreeTy ty]
            Nothing -> mempty
            {-
          metaChildren = case e ^. _exprMetaLens % _type of
            Nothing -> mempty
            Just (TCChkedAt ty) -> [binderTreeTy ty]
            Just (TCSynthed ty) -> [binderTreeTy ty]
            Just (TCEmb (TCBoth ty1 ty2)) -> [binderTreeTy ty1, binderTreeTy ty2]
-} -- don't include metadata. see #556
      in Node (Just e) $ exprChildren <> map ((mempty,). noNodeLabels) (typeChildren {- <> metaChildren-})

data Shadowing = ShadowingExists | ShadowingNotExists
  deriving (Eq, Show)

checkShadowing :: Tree () (Set Name) -> Shadowing
checkShadowing t = if fst $ foldTree f t
  then ShadowingExists
  else ShadowingNotExists
  where
    f :: () -> [(Set Name,(Bool,Set Name))] -> (Bool,Set Name)
    f () xs = let allSubtreeBinds = Set.unions $ map (snd.snd) xs
                  bindsHere = Set.unions $ map fst xs
                  allBinds = bindsHere <> allSubtreeBinds
                  shadowing = any (\(bs, (s, bs')) -> s || not (Set.disjoint bs bs')) xs
              in (shadowing, allBinds)


-- TODO: We check that all advertised API calls never introduce shadowing
-- - actionsForDef,
-- - actionsForDefBody,
-- - actionsForDefSig,

unit_shadow_action_a :: Assertion
unit_shadow_action_a =
  let (e,t) = create' $ (,) <$> hole emptyHole <*> tEmptyHole
      mn = ModuleName ["M", "0"]
      m = Module mn mempty $
            Map.singleton "a" $ DefAST $ ASTDef e t
      p = App.Prog   { progImports = []
                     , progModules = [m]
                     , progSelection = Nothing
                     , progSmartHoles = SmartHoles
                     , progLog = App.Log []
                     }
      a' = App.mkAppSafe (toEnum 0) p
  in do
    a <- case a' of
      Left _ -> assertFailure "bad app"
      Right a'' -> pure a''
    case runEditAppM (handleEditRequest
                      [CreateDef mn $ Just "aCopy"
                      ,CopyPasteSig (qualifyDefName m "a",getID t) []
                      ,CopyPasteBody (qualifyDefName m "a",getID e) []])
         a of
      (Left err, _ ) -> assertFailure $ show err
      (Right _, _) -> pure ()

-- TODO: this actually just tests actions work -- should be moved!
tasty_shadow_action :: Property
tasty_shadow_action = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      a <- forAllT $ genApp [builtinModule, primitiveModule]
      (m,(defName, def')) <- forAllT $ Gen.justT $ do
        m' <- Gen.element $ progModules $ appProg a
        let ds = Map.toList $ moduleDefsQualified m'
        traverse (fmap (m',) . element) $ nonEmpty ds
      def <- case def' of
        DefAST d -> pure d
        _ -> discard
      -- TODO: other sorts of action...
      act <- forAllWithT name' $ Gen.element $ actionsForDef l (moduleDefsQualified m) (defName, def)
      case input act of
--        InputRequired a' -> _
        NoInputRequired act' -> annotateShow act' >> actionSucceeds (handleEditRequest act') a
--        AskQuestion q a' -> _
        _ -> discard
      {-
      let globs = foldMap moduleDefsQualified testModules
      tds <- asks typeDefs
      (dir, t, ty) <- genDirTm
      unless (noShadowing t == ShadowingNotExists) discard
      unless (noShadowingTy ty == ShadowingNotExists) discard
      annotateShow t
      n <- forAllT (qualifyName <$> genModuleName <*> genName)
      l <- forAllT $ Gen.element enumerate
      i <- forAllT $ Gen.element $ t ^.. exprIDs
      a <- forAllWithT name' $ Gen.element $ actionsForDefBody l n i t
      case input a of
--        InputRequired a' -> _
        NoInputRequired a' -> case a' of
--          [MoveToDef m , BodyAction as'] | n == m -> do
--             _
          [] -> footnote "actionsForDefBody always returns a MoveToDef as first action, and rest of actions are wrapped in BodyAction" >> failure
--        AskQuestion q a' -> _
        _ -> discard
-}
  where
    name' a = toS $ case name a of
      Code t -> t
      Prose t -> t
    actionSucceeds :: HasCallStack => EditAppM a -> App.App -> PropertyT WT ()
    actionSucceeds m a = case runEditAppM m a of
      (Left err, _) -> annotateShow err >> failure
      (Right _, _) -> pure ()
    element = Gen.element . toList
      
-- Check evaluation does not introduce shadowing, except in some known cases
tasty_eval_shadow :: Property
tasty_eval_shadow = withTests 500 $
  withDiscards 2000 $
    propertyWTInExtendedGlobalCxt [builtinModule, primitiveModule] $ do
      let globs = foldMap moduleDefsQualified testModules
      tds <- asks typeDefs
      (dir, t, ty) <- genDirTm
      unless (noShadowing t == ShadowingNotExists) discard
      unless (noShadowingTy ty == ShadowingNotExists) discard
      when (any isKnownShadow $ U.universe t) discard
      (_steps, s) <- evalFullStepCount tds globs 1 dir t
      annotateShow s
      noShadowing (getEvalResultExpr s) === ShadowingNotExists
  where
    -- There are a few cases where evaluation may cause shadowing
    -- currently
    isKnownShadow e = isLet e || isHetroAPP e || isKnownCase e
    -- Since we inline let bindings underneath the let (without
    -- simultaneously removing the let binding), this can easily
    -- cause shadowing. If we implement a "push down let bindings"
    -- explicit-substitution style rule, then this check can be
    -- removed. See https://github.com/hackworthltd/primer/issues/44
    isLet = \case
      Let{} -> True
      Letrec{} -> True
      LetType{} -> True
      _ -> False
    -- Since the rule here is (because we do not have the ability
    -- to put a `lettype` inside a type)
    -- (Λa.e : ∀b.T) S ~> lettype b=S in (lettype a = S in e) : T
    -- it could happen that the `lettype b` may shadow something in `e`
    isHetroAPP = \case
      APP _ (Ann _ (LAM _ x _) (TForall _ y _ _)) _ -> x /= y
      _ -> False
    -- Since we introduce a let bindings per argument, and annotate
    -- these with the type from the type declaration, we could
    -- introduce a shadowed binder. For instance, if we have
    --   λx. case (C a : D) of C t -> t
    -- it will evaluate to
    --   λx. let t = a : A in t
    -- where 'A' is read from the declaration of 'D', and thus may
    -- contain a ∀ x. ...
    isKnownCase = \case
      Case _ e _ | (h,_) <- unfoldApp e, (Con{},_) <- unfoldAPP h -> True
      _ -> False

unit_known_case_shadow :: Assertion
unit_known_case_shadow =
  let ((expr, expected), maxID) = create $ do
        e <- lam "x" $ case_ ((con' ["M"] "C" `app` emptyHole) `ann` tcon' ["M"] "D")
                             [branch' (["M"],"C") [("t",Nothing)] emptyHole]
        expect <- lam "x" $ let_ "t" (emptyHole `ann` tforall "x" KType (tvar "x")) emptyHole
        pure (e, expect)
      td = TypeDefAST $ ASTTypeDef {
              astTypeDefParameters = mempty
              , astTypeDefConstructors = [ValCon (vcn ["M"] "C") [TForall () "x" KType $ TVar () "x"]]
              , astTypeDefNameHints = mempty}
      s = evalFullTest maxID (Map.singleton (tcn ["M"] "D") td) mempty 1 Chk expr
   in do
        distinctIDs s
        s <~==> Left (TimedOut expected)
        noShadowing expected @?= ShadowingNotExists

unit_known_case_shadow_substTy :: Assertion
unit_known_case_shadow_substTy =
  let ((expr, expected), maxID) = create $ do
        let v :: LocalName k
            v = "a25"
        e <- lAM "x" $ case_ ((con' ["M"] "C" `app` emptyHole `app` emptyHole) `ann` (tcon' ["M"] "D" `tapp` tvar "x"))
                             [branch' (["M"],"C") [(v,Nothing),("t",Nothing)] emptyHole]
        expect <- lAM "x" $ let_ v (emptyHole `ann` tcon tBool) $
           let_ "t" (emptyHole `ann` tforall v KType (tvar "x")) emptyHole
        pure (e, expect)
      td = TypeDefAST $ ASTTypeDef {
              astTypeDefParameters = [("p", KType)]
              , astTypeDefConstructors = [ValCon (vcn ["M"] "C") [TCon () tBool,
                                                                  TForall () "x" KType $ TVar () "p"]]
              , astTypeDefNameHints = mempty}
      s = evalFullTest maxID (Map.singleton (tcn ["M"] "D") td) mempty 1 Chk expr
   in do
        distinctIDs s
        s <~==> Left (TimedOut expected)
        noShadowing expected @?= ShadowingNotExists

getEvalResultExpr :: Either EvalFullError Expr -> Expr
getEvalResultExpr = \case
  Left (TimedOut e) -> e
  Right e -> e
