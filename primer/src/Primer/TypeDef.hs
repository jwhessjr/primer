module Primer.TypeDef (
  TypeDef (..),
  _TypeDefAST,
  ValCon (..),
  _valConArgs,
  TypeDefMap,
  typeDefAST,
  typeDefKind,
  ASTTypeDef (..),
  _astTypeDefConstructors,
  valConType,
  _typedefFields,
  forgetTypeDefMetadata,
) where

import Foreword

import Data.Data (Data)
import Optics (Traversal, over, traversed, (%), traversalVL, Lens, lens)
import Primer.Core.Meta (
  TyConName,
  ValConName,
 )
import Primer.Core.Transform (mkTAppCon)
import Primer.Core.Type (
  Kind (KType),
  Type' (TFun),
 )
import Primer.Core.Utils (forgetTypeMetadata)

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
  { astTypeDefConstructors :: [ValCon b]
  }
  deriving stock (Eq, Show, Read, Data)

_astTypeDefConstructors :: Lens (ASTTypeDef b) (ASTTypeDef b') [ValCon b] [ValCon b']
_astTypeDefConstructors = lens astTypeDefConstructors $ \d cs -> d {astTypeDefConstructors = cs}

data ValCon b = ValCon
  { valConName :: ValConName
  , valConArgs :: [Type' b]
  }
  deriving stock (Eq, Show, Read, Data)

_valConArgs :: Lens (ValCon b) (ValCon b') [Type' b] [Type' b']
_valConArgs = lens valConArgs $ \c as -> c {valConArgs = as}

valConType :: TyConName -> ASTTypeDef () -> ValCon () -> Type' ()
valConType tc _td vc =
  let ret = mkTAppCon tc []
      args = foldr (TFun ()) ret (forgetTypeMetadata <$> valConArgs vc)
   in args

typeDefAST :: TypeDef b -> Maybe (ASTTypeDef b)
typeDefAST = \case
  TypeDefAST t -> Just t
typeDefKind :: TypeDef b -> Kind
typeDefKind _ =  KType

-- | A traversal over the contstructor fields in an typedef.
_typedefFields :: Traversal (TypeDef b) (TypeDef c) (Type' b) (Type' c)
_typedefFields = _TypeDefAST % _astTypeDefConstructors % traversed % _valConArgs % traversed

forgetTypeDefMetadata :: TypeDef b -> TypeDef ()
forgetTypeDefMetadata = over _typedefFields forgetTypeMetadata
