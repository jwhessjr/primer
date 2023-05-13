module TypeDef (
  TypeDef (..),
  TypeDefMap,
  typeDefKind,
  ASTTypeDef (..),
  forgetTypeDefMetadata,
) where

import Foreword

import Data.Data (Data)
import Type (
  Kind (KType),
 )

data TypeDef b
  = TypeDefAST (ASTTypeDef b)
  deriving stock (Eq, Show, Read, Data)

-- | A mapping of global names to 'TypeDef's.
type TypeDefMap = Map Text (TypeDef ())

-- | Definition of an algebraic data type
--
-- Consider the type T = ASTTypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data ASTTypeDef b = ASTTypeDef
  deriving stock (Eq, Show, Read, Data)

typeDefKind :: TypeDef b -> Kind
typeDefKind _ = KType

forgetTypeDefMetadata :: TypeDef b -> TypeDef ()
forgetTypeDefMetadata = const $ TypeDefAST ASTTypeDef
