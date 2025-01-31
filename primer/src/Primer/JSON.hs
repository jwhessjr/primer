module Primer.JSON (
  CustomJSON (..),
  PrimerJSON,
  ToJSON,
  FromJSON,
  ToJSONKey,
  FromJSONKey,
) where

import Data.Aeson (
  FromJSON,
  FromJSONKey,
  ToJSON,
  ToJSONKey,
 )
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Vanilla)

-- | A type for Primer API JSON encodings.
--
-- Note: at the moment, this makes no changes to the default
-- 'FromJSON' and 'ToJSON' encodings. We keep it a) to get the more
-- efficient generic encodings generated by @deriving-aeson@, and b)
-- to provide an escape hatch if we need to add custom encodings in
-- the future.
--
-- Note: some options for 'CustomJSON' are not appropriate:
--
-- * @ConstructorTagModifier@ takes options. Some of these, like @CamelTo*@ can
--   collapse constructors and result in an ambiguous encoding.
--   This is particularly fraught since we have constructors that differ only
--   in case.
--
-- * @SumUntaggedValue@ also results in ambiguous encodings in general. For
--   example @data T = A | B String@ will encode both @A@ and @B "A"@
--   identically.
--
-- Other options are incompatible with openapi3:
--
-- * @TagSingleConstructors@ is ignored. It will change the encoding but not the
--   schema, and will result in an inconsistency between the two.
--   See https://github.com/biocad/openapi3/issues/55.
--
-- * @SumTwoElemArray@ is unsupported by openapi3 as it is unrepresentable in
--   a schema.
type PrimerJSON a = Vanilla a
