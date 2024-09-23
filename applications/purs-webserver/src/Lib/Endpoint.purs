module Lib.Endpoint where

import Data.Tuple.Nested (get1, get2, get3)
import Lib (Endpoint, Method, Route, Handler)

getMethod :: Endpoint -> Method
getMethod = get1

getRoute :: Endpoint -> Route
getRoute = get2

getHandler :: Endpoint -> Handler
getHandler = get3