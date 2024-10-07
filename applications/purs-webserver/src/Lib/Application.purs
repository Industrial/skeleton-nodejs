module Lib.Application where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Lib (Application, Endpoint)
import Lib.Endpoint (getHandler, getMethod, getRoute)
import Lib.HTTPRequest as HTTPRequest
import Lib.HTTPResponse as HTTPResponse
import Lib.Route (matchesURL)
import Lib.Stream (toBuffer)
import Lib.String (stripTrailingSlash)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (Read, Stream)

handleListening :: Int -> String -> ((Either Error Unit -> Effect Unit)) -> Effect Unit
handleListening port hostname done = do
  logShow ("Server running at http://" <> hostname <> ":" <> (show port))
  done $ Right unit

listen :: Int -> String -> Application -> Effect Unit
listen port hostname app =
  launchAff_ $ makeAff \done -> do
    server <- HTTP.createServer $ handleRequest app
    HTTP.listen server
      { backlog: Just 1
      , hostname: hostname
      , port: port
      }
      (handleListening port hostname done)
    pure nonCanceler

parseRequestBody :: Stream (read ∷ Read) -> Aff String
parseRequestBody requestStream = do
  buffer <- toBuffer requestStream
  liftEffect $ Buffer.toString UTF8 buffer

handleRequest :: Application -> HTTP.Request -> HTTP.Response -> Effect Unit
handleRequest app request response = do
  let method = HTTPRequest.requestMethod request
  logShow ("Request Method: " <> method)
  let url = HTTP.requestURL request
  logShow ("Request URL: " <> url)
  let matchingEndpoint = findMatchingEndpoint method url app
  launchAff_ do
    case matchingEndpoint of
      Just endpoint -> do
        -- let middleware = getMiddleware endpoint
        let handler = getHandler endpoint
        -- runMiddlewareChain middleware request response handler
        handler request response
      _ -> do
        HTTPResponse.notFound request response

findMatchingEndpoint :: String -> String -> Application -> Maybe Endpoint
findMatchingEndpoint method url app =
  Array.find
    ( \endpoint ->
        let
          endpointMethod = getMethod endpoint
          endpointRoute = getRoute endpoint
          strippedURL = stripTrailingSlash url
        in
          endpointMethod == method && matchesURL endpointRoute strippedURL
    )
    app
