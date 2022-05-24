module Tests.AlphaEquality where

import Foreword

import Gen.Core.Raw (
  evalExprGen,
  genTyVarName,
  genType,
 )
import Hedgehog hiding (check)
import Primer.Builtins
import Primer.Core (
  Kind (KFun, KType),
  Type',
 )
import Primer.Core.DSL
import Primer.Core.Utils (alphaEqTy, forgetTypeIDs)
import Test.Tasty.HUnit hiding (assert)

unit_1 :: Assertion
unit_1 =
  assertNotEqual
    (create' (tcon tNat))
    (create' (tcon tBool))

unit_2 :: Assertion
unit_2 =
  (@?=)
    (create' (tcon tList `tapp` tcon tNat))
    (create' (tcon tList `tapp` tcon tNat))

unit_3 :: Assertion
unit_3 =
  assertNotEqual
    (create' (tcon tList `tapp` tcon tBool))
    (create' (tcon tList `tapp` tcon tNat))

unit_4 :: Assertion
unit_4 =
  assertNotEqual
    (create' (tcon tList `tapp` tcon tBool))
    (create' (tcon tNat))

unit_5 :: Assertion
unit_5 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon tList `tapp` tvar "a"))
    (create' (tcon tNat))

unit_6 :: Assertion
unit_6 =
  (@?=)
    (create' (tforall "a" KType $ tcon tList `tapp` tvar "a"))
    (create' (tforall "b" KType $ tcon tList `tapp` tvar "b"))

unit_7 :: Assertion
unit_7 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon tList `tapp` tvar "a"))
    (create' (tforall "b" KType $ tcon tList `tapp` tcon tBool))

unit_8 :: Assertion
unit_8 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon tBool))
    (create' (tforall "b" (KFun KType KType) $ tcon tBool))

unit_9 :: Assertion
unit_9 =
  assertNotEqual
    (create' (tforall "a" KType $ tforall "b" KType $ tcon tList `tapp` tvar "a"))
    (create' (tforall "a" KType $ tforall "b" KType $ tcon tList `tapp` tvar "b"))

unit_10 :: Assertion
unit_10 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon tList `tapp` tvar "a"))
    (create' (tcon tList `tapp` tforall "a" KType (tvar "b")))

unit_11 :: Assertion
unit_11 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon tBool `tfun` (tcon tList `tapp` tvar "a")))
    (create' (tcon tBool `tfun` tforall "a" KType (tcon tList `tapp` tvar "a")))

hprop_refl :: Property
hprop_refl = property $ do
  t <- forgetTypeIDs <$> forAll (evalExprGen 0 genType)
  assert $ alphaEqTy t t

hprop_alpha :: Property
hprop_alpha = property $ do
  s <- f <$> forAll (evalExprGen 0 genTyVarName)
  t <- f <$> forAll (evalExprGen 0 genTyVarName)
  s === t
  where
    f v = create' $ tforall v KType $ tvar v

create' :: S (Type' a) -> Alpha
create' = Alpha . forgetTypeIDs . fst . create

-- | Like @Type' ()@, but 'Eq' only compares up to alpha-equality.
newtype Alpha = Alpha (Type' ())
  deriving (Show)

instance Eq Alpha where
  (Alpha x) == (Alpha y) = x `alphaEqTy` y

assertNotEqual :: Alpha -> Alpha -> Assertion
assertNotEqual s t = assertBool "types are equal" $ s /= t
