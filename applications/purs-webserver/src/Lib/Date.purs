module Lib.Date where

import Prelude

import Data.JSDate (JSDate, getTime)

differenceInMilliseconds :: JSDate -> JSDate -> Number
differenceInMilliseconds b a = (getTime b) - (getTime a)
