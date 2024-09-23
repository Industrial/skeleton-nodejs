module Lib.Middleware where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, parseJson, stringify)
import Data.Either (Either(..))
import Data.JSDate (JSDate, now)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Lib (Handler)
import Lib.Buffer (toString)
import Lib.Date (differenceInMilliseconds)
import Lib.Stream (toBuffer)
import Node.HTTP (Request, Response, requestAsStream)

responseTimeBefore :: Request -> Response -> Aff JSDate
responseTimeBefore _ _ = do
  before <- liftEffect now
  pure $ before

responseTimeAfter :: JSDate -> Request -> Response -> Aff Unit
responseTimeAfter before _ _ = do
  after <- liftEffect now
  let difference = differenceInMilliseconds after before
  liftEffect $ Console.log $ (show difference) <> "ms"

responseTime :: Handler -> Handler
responseTime handler =
  ( \request response -> do
      before <- responseTimeBefore request response
      handler request response
      responseTimeAfter before request response
  ) :: Handler

requestBodyParser :: Request -> Response -> Aff String
requestBodyParser request _ = do
  let requestStream = requestAsStream request
  buffer <- toBuffer requestStream
  liftEffect $ toString buffer

requestBodyJSONParser :: Request -> Response -> Aff (Either JsonDecodeError Json)
requestBodyJSONParser request response = do
  body <- requestBodyParser request response
  let output = parseJson body
  case output of
    Left error -> do
      pure $ Left error
    Right json -> do
      pure $ Right json

responseBodyJSONSerializer :: Json -> Request -> Response -> Aff String
responseBodyJSONSerializer json _ _ = do
  let jsonString = stringify json
  pure jsonString

-- requestMultipartBodyParserMiddleware :: Handler
-- requestCookieParserMiddleware :: Handler
-- responseCORSHeaderSerializerMiddleware :: Handler
-- responseJSONBodySerializerMiddleware :: Handler
