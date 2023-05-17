module Primer.TypeDef (
  TypeDef (..),
  ValCon (..),
  TypeDefMap,
  typeDefKind,
  typeDefParameters,
  ASTTypeDef (..),
  PrimTypeDef (..),
  valConType,
) where

import Foreword

import Data.Data (Data)
import Primer.Core.Meta (
  TyConName,
  TyVarName,
  ValConName,
 )
import Primer.Core.Transform (mkTAppCon)
import Primer.Core.Type (
  Kind (KFun, KType),
  Type' (TForall, TFun, TVar),
 )
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Name (Name)

data TypeDef b
  = TypeDefPrim PrimTypeDef
  | TypeDefAST (ASTTypeDef b)
  deriving stock (Eq, Show, Read, Data)

-- | A mapping of global names to 'TypeDef's.
type TypeDefMap = Map TyConName (TypeDef ())

-- | Definition of a primitive data type
data PrimTypeDef = PrimTypeDef
  { primTypeDefParameters :: [Kind]
  , primTypeDefNameHints :: [Name]
  }
  deriving stock (Eq, Show, Read, Data)

-- | Definition of an algebraic data type
--
-- Consider the type T = ASTTypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data ASTTypeDef b = ASTTypeDef
  { astTypeDefParameters :: [(TyVarName, Kind)] -- These names scope over the constructors
  , astTypeDefConstructors :: [ValCon b]
  , astTypeDefNameHints :: [Name]
  }
  deriving stock (Eq, Show, Read, Data)

data ValCon b = ValCon
  { valConName :: ValConName
  , valConArgs :: [Type' b]
  }
  deriving stock (Eq, Show, Read, Data)

valConType :: TyConName -> ASTTypeDef () -> ValCon () -> Type' ()
valConType tc td vc =
  let ret = mkTAppCon tc (TVar () . fst <$> astTypeDefParameters td)
      args = foldr (TFun ()) ret (forgetTypeMetadata <$> valConArgs vc)
      foralls = foldr (\(n, k) t -> TForall () n k t) args (astTypeDefParameters td)
   in foralls

typeDefParameters :: TypeDef b -> [Kind]
typeDefParameters = \case
  TypeDefPrim t -> primTypeDefParameters t
  TypeDefAST t -> snd <$> astTypeDefParameters t

typeDefKind :: TypeDef b -> Kind
typeDefKind = foldr KFun KType . typeDefParameters
