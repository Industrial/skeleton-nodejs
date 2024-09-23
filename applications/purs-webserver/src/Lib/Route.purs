module Lib.Route where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.String as String
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (RegexFlags(..))
import Data.Tuple (Tuple(..))
import Lib (Route)

newtype Parameter = Parameter String

parameterRegexFlags :: RegexFlags
parameterRegexFlags =
  RegexFlags
    { global: true
    , ignoreCase: true
    , multiline: false
    , dotAll: false
    , sticky: false
    , unicode: false
    }

isParameter :: String -> Boolean
isParameter str =
  case regex "^:[A-Za-z0-9_]+$" parameterRegexFlags of
    Left _ -> false
    Right re -> test re str

matchesURL :: Route -> String -> Boolean
matchesURL route url =
  let
    urlParts = String.split (String.Pattern "/") url
    routeParts = String.split (String.Pattern "/") route
    pairs = Array.zip urlParts routeParts
    matches' = map (\(Tuple u p) -> u == p || isParameter p) pairs
  in
    Array.all identity matches'

parametersOfRoute :: Route -> String -> Map.Map String String
parametersOfRoute route url =
  let
    urlParts = String.split (String.Pattern "/") url
    routeParts = String.split (String.Pattern "/") route
    pairs = Array.zip urlParts routeParts
    paramPairs = Array.filter (\(Tuple _ p) -> isParameter p) pairs
    params = map (\(Tuple u p) -> Tuple (String.drop 1 p) u) paramPairs
  in
    Map.fromFoldable params
