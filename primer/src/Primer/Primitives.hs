{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (
  PrimDef (..),
  allPrimTypeDefs,
  tInt,
  tChar,
  primitive,
  primConName,
  primDefName,
  primDefType,
  PrimFunError (..),
  primitiveModuleName,
) where

import Foreword

import Data.Data (Data)
import Data.Map qualified as M
import Primer.Builtins (
  tBool,
  tMaybe,
  tNat,
 )
import Primer.Core (
  Expr',
  GlobalName,
  ModuleName,
  PrimCon (PrimChar, PrimInt),
  TyConName,
  Type' (..),
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Name (Name)
import Primer.Primitives.PrimDef (PrimDef (..))
import Primer.TypeDef (PrimTypeDef (..))

data PrimFunError
  = -- | We have attempted to apply a primitive function to invalid args.
    PrimFunError
      PrimDef
      [Expr' () ()]
      -- ^ Arguments
  deriving stock (Eq, Show, Data, Generic)

primitiveModuleName :: ModuleName
primitiveModuleName = mkSimpleModuleName "Primitives"

-- | The name of the type to which this primitive constructor belongs.
-- This should be a key in `allPrimTypeDefs`.
primConName :: PrimCon -> TyConName
primConName = \case
  PrimChar _ -> tChar
  PrimInt _ -> tInt

primitive :: Name -> GlobalName k
primitive = qualifyName primitiveModuleName

tChar :: TyConName
tChar = primitive "Char"

tInt :: TyConName
tInt = primitive "Int"

-- | Primitive type definitions.
-- There should be one entry here for each constructor of `PrimCon`.
allPrimTypeDefs :: Map TyConName PrimTypeDef
allPrimTypeDefs =
  M.fromList
    [ let name = tChar
       in ( name
          , PrimTypeDef
              { primTypeDefParameters = []
              , primTypeDefNameHints = ["c"]
              }
          )
    , let name = tInt
       in ( name
          , PrimTypeDef
              { primTypeDefParameters = []
              , primTypeDefNameHints = ["i", "j", "k", "m", "n"]
              }
          )
    ]
  where
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this map.
    _ = \case
      PrimChar _ -> ()
      PrimInt _ -> ()

primDefName :: PrimDef -> Name
primDefName = \case
  ToUpper -> "toUpper"
  IsSpace -> "isSpace"
  HexToNat -> "hexToNat"
  NatToHex -> "natToHex"
  EqChar -> "eqChar"
  IntAdd -> "Int.+"
  IntMinus -> "Int.-"
  IntMul -> "Int.×"
  IntQuotient -> "Int.quotient"
  IntRemainder -> "Int.remainder"
  IntQuot -> "Int.quot"
  IntRem -> "Int.rem"
  IntLT -> "Int.<"
  IntLTE -> "Int.≤"
  IntGT -> "Int.>"
  IntGTE -> "Int.≥"
  IntEq -> "Int.="
  IntNeq -> "Int.≠"
  IntToNat -> "Int.toNat"
  IntFromNat -> "Int.fromNat"

primDefType :: PrimDef -> Type' ()
primDefType = uncurry (flip $ foldr $ TFun ()) . primFunTypes

primFunTypes :: PrimDef -> ([Type' ()], Type' ())
primFunTypes = \case
  ToUpper -> ([c tChar], c tChar)
  IsSpace -> ([c tChar], c tBool)
  HexToNat -> ([c tChar], c tMaybe `a` c tNat)
  NatToHex -> ([c tNat], c tMaybe `a` c tChar)
  EqChar -> ([c tChar, c tChar], c tBool)
  IntAdd -> ([c tInt, c tInt], c tInt)
  IntMinus -> ([c tInt, c tInt], c tInt)
  IntMul -> ([c tInt, c tInt], c tInt)
  IntQuotient -> ([c tInt, c tInt], c tMaybe `a` c tInt)
  IntRemainder -> ([c tInt, c tInt], c tMaybe `a` c tInt)
  IntQuot -> ([c tInt, c tInt], c tInt)
  IntRem -> ([c tInt, c tInt], c tInt)
  IntLT -> ([c tInt, c tInt], c tBool)
  IntLTE -> ([c tInt, c tInt], c tBool)
  IntGT -> ([c tInt, c tInt], c tBool)
  IntGTE -> ([c tInt, c tInt], c tBool)
  IntEq -> ([c tInt, c tInt], c tBool)
  IntNeq -> ([c tInt, c tInt], c tBool)
  IntToNat -> ([c tInt], c tMaybe `a` c tNat)
  IntFromNat -> ([c tNat], c tInt)
  where
    c = TCon ()
    a = TApp ()
