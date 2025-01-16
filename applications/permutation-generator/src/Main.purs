module Main where

import Data.Array as Array
import Effect (Effect)
import Effect.Console (logShow)
import Prelude

-- | `numberLocksPermutations x y` gives you all permutations of length `y`
-- | using numbers from `0` to `x`.
-- |
-- | ```purescript
-- | numberLocksPermutations 2 1 == [[0], [1], [2]]
-- | numberLocksPermutations 2 2 == [[0,0], [0,1], [0,2], [1,0], [1,1], [1,2], [2,0], [2,1], [2,2]]
-- | numberLocksPermutations 3 2 == [[0,0], [0,1], [0,2], [0,3], [1,0], [1,1], [1,2], [1,3], [2,0], [2,1], [2,2], [2,3], [3,0], [3,1], [3,2], [3,3]]
-- | ```
-- |
-- | *Time complexity: O((x+1)^y).*
numberLocksPermutations :: Int -> Int -> Array (Array Int)
numberLocksPermutations _ 0 = [ [] ]
numberLocksPermutations x len = do
  prefix <- Array.range 0 x
  rest <- numberLocksPermutations x (len - 1)
  pure (Array.cons prefix rest)

main :: Effect Unit
main = do
  let result = numberLocksPermutations 9 3
  logShow result
