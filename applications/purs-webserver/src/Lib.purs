module Lib where

import Prelude

import Data.Tuple.Nested (Tuple3)
import Effect.Aff (Aff)
import Node.HTTP (Request, Response)

type Handler = Request -> Response -> Aff Unit

type Method = String

type Route = String

type Endpoint = Tuple3 Method Route Handler

type Application = Array Endpoint
