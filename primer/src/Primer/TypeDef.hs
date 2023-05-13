module Primer.TypeDef (
  TypeDef (..),
  _TypeDefAST,
  TypeDefMap,
  typeDefAST,
  typeDefKind,
  ASTTypeDef (..),
  _typedefFields,
  forgetTypeDefMetadata,
) where

import Foreword

import Data.Data (Data)
import Optics (Traversal, traversalVL)
import Primer.Core.Meta (
  TyConName,
 )
import Primer.Core.Type (
  Kind (KType),
  Type',
 )

data TypeDef b
  = TypeDefAST (ASTTypeDef b)
  deriving stock (Eq, Show, Read, Data)

_TypeDefAST :: Traversal (TypeDef b) (TypeDef b') (ASTTypeDef b) (ASTTypeDef b')
_TypeDefAST = traversalVL $ \f -> \case
  TypeDefAST x -> TypeDefAST <$> f x

-- | A mapping of global names to 'TypeDef's.
type TypeDefMap = Map TyConName (TypeDef ())

-- | Definition of an algebraic data type
--
-- Consider the type T = ASTTypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data ASTTypeDef b = ASTTypeDef
  deriving stock (Eq, Show, Read, Data)

typeDefAST :: TypeDef b -> Maybe (ASTTypeDef b)
typeDefAST = \case
  TypeDefAST t -> Just t
typeDefKind :: TypeDef b -> Kind
typeDefKind _ =  KType

-- | A traversal over the contstructor fields in an typedef.
_typedefFields :: Traversal (TypeDef b) (TypeDef c) (Type' b) (Type' c)
_typedefFields = traversalVL $ \_ _ -> pure $ TypeDefAST ASTTypeDef

forgetTypeDefMetadata :: TypeDef b -> TypeDef ()
forgetTypeDefMetadata = const $ TypeDefAST ASTTypeDef
