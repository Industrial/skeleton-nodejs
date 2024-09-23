module Lib.Array where

import Prelude

import Data.Array (filter, (:))

without :: forall a. Eq a => a -> Array a -> Array a
without a as = filter (_ /= a) as

with :: forall a. Eq a => a -> Array a -> Array a
with a as = a : without a as