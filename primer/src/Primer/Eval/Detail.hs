module Primer.Eval.Detail (
  EvalDetail,
) where

import Foreword

import Primer.Core (Expr)
import Primer.Core.Meta (LocalNameKind (..))
import Primer.Core.Type (Kind, Type)
import Primer.Eval.Ann ( RemoveAnnDetail )
import Primer.Eval.Beta ( BetaReductionDetail )
import Primer.Eval.Bind ( BindRenameDetail )
import Primer.Eval.Case ( CaseReductionDetail )
import Primer.Eval.Inline
    ( GlobalVarInlineDetail, LocalVarInlineDetail )
import Primer.Eval.Let ( LetRemovalDetail )
import Primer.Eval.Prim ( ApplyPrimFunDetail )

-- | Detailed information about a reduction step
data EvalDetail
  = -- | Reduction of (λx. a : S -> T) b
    BetaReduction (BetaReductionDetail 'ATmVar Type Type)
  | -- | Reduction of (Λx. a : ∀y:k. T) S
    BETAReduction (BetaReductionDetail 'ATyVar Kind Type)
  | -- | Inlining of a local (let-bound) variable
    LocalVarInline (LocalVarInlineDetail 'ATmVar)
  | -- | Inlining of a local (let-bound) type variable
    LocalTypeVarInline (LocalVarInlineDetail 'ATyVar)
  | -- | Inlining of a global variable (top-level definition)
    GlobalVarInline GlobalVarInlineDetail
  | -- | Removing a term-level @let@ whose bound variable is unused
    LetRemoval (LetRemovalDetail Expr)
  | -- | Removing a type-level @let@ whose bound variable is unused
    TLetRemoval (LetRemovalDetail Type)
  | -- | Renaming of binding in an expression
    BindRename (BindRenameDetail Expr)
  | -- | Renaming of binding in a type
    TBindRename (BindRenameDetail Type)
  | -- | Reduction of case-of-known-constructor
    CaseReduction CaseReductionDetail
  | -- | Elide annotation
    RemoveAnn RemoveAnnDetail
  | -- | Apply a primitive function
    ApplyPrimFun ApplyPrimFunDetail
  deriving stock (Eq, Show, Read, Generic)
