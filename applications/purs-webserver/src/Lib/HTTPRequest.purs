module Lib.HTTPRequest where

import Prelude

import Data.String (toUpper)
import Node.HTTP as HTTP

requestMethod :: HTTP.Request -> String
requestMethod request = toUpper $ HTTP.requestMethod request
