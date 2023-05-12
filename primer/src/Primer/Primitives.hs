{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (
  PrimDef (..),
  tInt,
  tChar,
  primitive,
  primConName,
  primDefType,
  PrimFunError (..),
  primitiveModuleName,
) where

import Foreword

import Data.Data (Data)
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
