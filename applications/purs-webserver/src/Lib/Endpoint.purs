module Lib.Endpoint where

import Data.Tuple.Nested (get1, get2, get3)
import Lib (Endpoint, Method, Route, Handler)

getMethod :: forall a. Endpoint a -> Method
getMethod = get1

getRoute :: forall a. Endpoint a -> Route
getRoute = get2

getHandler :: forall a. Endpoint a -> Handler a
getHandler = get3
