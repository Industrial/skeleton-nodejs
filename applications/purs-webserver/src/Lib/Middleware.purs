module Lib.Middleware
  ( runMiddlewareChain
  ) where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, parseJson, stringify)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.JSDate (JSDate, now)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Lib (Handler, Middleware, Context)
import Lib.Buffer (toString)
import Lib.Date (differenceInMilliseconds)
import Lib.Stream (toBuffer)
import Node.HTTP (Request, Response, requestAsStream)

responseTime :: forall a. Middleware a
responseTime ctx next = do
  before <- liftEffect now
  resultCtx <- next ctx
  after <- liftEffect now
  let difference = differenceInMilliseconds after before
  liftEffect $ Console.log $ (show difference) <> "ms"
  pure resultCtx

requestBodyParser
  :: forall a
   . Middleware a
requestBodyParser ctx next = do
  let requestStream = requestAsStream ctx.req
  buffer <- toBuffer requestStream
  requestBody <- liftEffect $ toString buffer
  let
    newContext =
      { req: ctx.req
      , res: ctx.res
      , body: requestBody
      }
  next newContext

-- requestBodyJSONParser
--   :: Middleware
--        { req :: Request
--        , res :: Response
--        , body :: String
--        }
-- requestBodyJSONParser ctx next = do
--   body <- requestBodyParser request (\req -> pure req)
--   case parseJson body of
--     Left error -> do
--       -- handle JSON parse error
--       -- Assume some logic to send back an error response
--       pure response
--     Right json -> do
--       next request

-- responseBodyJSONSerializer :: Json -> Middleware
-- responseBodyJSONSerializer json request next = do
--   let jsonString = stringify json
--   -- Assume some logic to attach jsonString to response or context
--   next request

-- runMiddlewareChain :: forall a b. MiddlewareChain a b -> a -> Request -> b -> Aff Response
-- runMiddlewareChain []     requestHandler request responseHandler finalHandler = finalHandler context
-- runMiddlewareChain (m:ms) requestHandler request responseHandler finalHandler = do
--   requestHandler (\request -> )
--     context (\newContext -> runMiddlewareChain ms newContext finalHandler)

runMiddlewareChain
  :: forall a
   . Array (Middleware a)
  -> Handler a
  -> Context a
  -> Aff (Context a)
runMiddlewareChain middlewares final ctx =
  case uncons middlewares of
    Nothing ->
      final ctx
    Just { head: x, tail: xs } ->
      x ctx (\ctx' -> runMiddlewareChain xs final ctx')
