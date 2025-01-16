module Lib.HTTPRequest where

import Prelude

import Data.String (toUpper)
import Node.HTTP.Types (ClientRequest)
import Node.HTTP.ClientRequest (method)

requestMethod :: ClientRequest -> String
requestMethod request = toUpper $ method request
