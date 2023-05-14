module Meta (
  HasID (..),
  getID,
) where

import Foreword

import Data.Generics.Uniplate.Zipper (Zipper, hole, replaceHole)
import Optics (
  Lens',
  equality',
  lens,
  set,
  view,
 )

-- | A class for types which have an ID.
-- This makes it easier to change the underlying metadata representation without
-- breaking code that needs to work with IDs, because they use this class
-- instead of hardcoding paths to IDs or using chained 'HasType' instances,
-- which can lead to ambiguity errors.
class HasID a where
  _id :: Lens' a Int

instance HasID Int where
  _id = equality'

-- This instance is used in 'Zipper', but it would be an orphan if we defined it there.
instance HasID a => HasID (Zipper a a) where
  _id = lens getter setter
    where
      getter = view _id . hole
      setter z i =
        let t = hole z
         in replaceHole (set _id i t) z

-- | Get the ID of the given expression or type
getID :: HasID a => a -> Int
getID = view _id
