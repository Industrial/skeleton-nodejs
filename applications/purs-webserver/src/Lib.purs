module Lib where

import Data.Tuple.Nested (Tuple3)
import Effect.Aff (Aff)
import Node.HTTP (Request, Response)

type Context a =
  { req :: Request
  , res :: Response
  | a
  }

type Handler a =
  Context a
  -> Aff (Context a)

type Middleware a =
  Context a
  -> (Context a -> Aff (Context a))
  -> Aff (Context a)

type Method = String

type Route = String

type Endpoint a = Tuple3 Method Route (Handler a)

type Application a = Array (Endpoint a)
