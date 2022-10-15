module Tests.Eval.Utils ((~=),(~~=),
                        genDirTm,testModules,failWhenSevereLogs) where

import Foreword

import Hedgehog ( PropertyT, MonadTest )
import Primer.Gen.Core.Typed
    ( genWTType, genSyn, genChk, forAllT, WT )
import Primer.Eval (Dir (Chk, Syn))
import Primer.Core (Expr, Type', Type, Kind (KType), ModuleName (ModuleName))
import qualified Hedgehog.Gen as Gen
import Primer.Module (Module (Module, moduleName, moduleTypes, moduleDefs), builtinModule, primitiveModule)
import qualified Data.Map as Map
import Primer.Def (ASTDef(ASTDef, astDefType, astDefExpr), Def (DefAST))
import Test.Tasty.HUnit (Assertion, (@?=))
import Primer.Core.Utils (generateIDs, forgetMetadata, forgetTypeMetadata)
import Primer.Core.DSL ( create', tcon, tfun, lam, lvar ) 
import Primer.Primitives (tChar)
import Primer.Log (PureLogT, runPureLogT)
import Control.Monad.Log (WithSeverity)
import Primer.EvalFull (EvalFullLog)
import TestUtils (testNoSevereLogs)

-- | Generates
--
--  * a term (to be the subject of some evaluation steps)
--
-- Also returns
--
--  * whether the term is synthesisable or checkable
--
--  * the type of the term
genDirTm :: PropertyT WT (Dir, Expr, Type' ())
genDirTm = do
  dir <- forAllT $ Gen.element [Chk, Syn]
  (t', ty) <- case dir of
    Chk -> do
      ty' <- forAllT $ genWTType KType
      t' <- forAllT $ genChk ty'
      pure (t', ty')
    Syn -> forAllT genSyn
  t <- generateIDs t'
  pure (dir, t, ty)

-- | Some generally-useful globals to have around when testing.
-- Currently: an AST identity function on Char and all builtins and
-- primitives
testModules :: [Module]
testModules = [builtinModule, primitiveModule, testModule]

testModule :: Module
testModule =
  let (ty, expr) = create' $ (,) <$> tcon tChar `tfun` tcon tChar <*> lam "x" (lvar "x")
   in Module
        { moduleName = ModuleName ["M"]
        , moduleTypes = mempty
        , moduleDefs =
            Map.singleton "idChar" $
              DefAST
                ASTDef
                  { astDefType = ty
                  , astDefExpr = expr
                  }
        }


-- * Misc helpers

-- | Like '@?=' but specifically for expressions.
-- Ignores IDs and metadata.
(~=) :: HasCallStack => Expr -> Expr -> Assertion
x ~= y = forgetMetadata x @?= forgetMetadata y

-- | Like '~=' but for types.
(~~=) :: HasCallStack => Type -> Type -> Assertion
x ~~= y = forgetTypeMetadata x @?= forgetTypeMetadata y

failWhenSevereLogs :: MonadTest m => PureLogT (WithSeverity EvalFullLog) m a -> m a
failWhenSevereLogs m = do
  (r, logs) <- runPureLogT m
  testNoSevereLogs logs
  pure r

