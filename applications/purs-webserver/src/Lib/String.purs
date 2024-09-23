module Lib.String where

import Prelude

import Data.String as String

startsWith :: String -> String -> Boolean
startsWith match str = String.take 0 str == match

endsWith :: String -> String -> Boolean
endsWith match str = String.take (String.length str) str == match

startsWithSlash :: String -> Boolean
startsWithSlash = startsWith "/"

endsWithSlash :: String -> Boolean
endsWithSlash = endsWith "/"

stripTrailingSlash :: String -> String
stripTrailingSlash str =
  case endsWithSlash str of
    true -> String.drop (String.length str - 1) str
    false -> str