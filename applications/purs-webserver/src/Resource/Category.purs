module Resource.Category where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, encodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Domain.Category as Category
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Lib (Handler)
import Lib.HTTPResponse as HTTPResponse
import Lib.Middleware as Middleware
import Node.HTTP (Request, Response)

type EndpointError =
  { error :: String
  }

encodeEndpointError :: EndpointError -> Json
encodeEndpointError = encodeJson

handleJsonParseError :: JsonDecodeError -> Request -> Response -> Aff Unit
handleJsonParseError error request response = do
  let errorMessage = printJsonDecodeError error
  let endpointError = encodeEndpointError { error: errorMessage }
  responseBody <- Middleware.responseBodyJSONSerializer endpointError request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest (Just responseBody) request response
  liftEffect $ Console.log $ errorMessage

handleJsonDecodeError :: JsonDecodeError -> Request -> Response -> Aff Unit
handleJsonDecodeError error request response = do
  let errorMessage = printJsonDecodeError error
  let endpointError = encodeEndpointError { error: errorMessage }
  responseBody <- Middleware.responseBodyJSONSerializer endpointError request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest (Just responseBody) request response
  liftEffect $ Console.log $ printJsonDecodeError error

createCategory :: Handler
createCategory request response = do
  before <- Middleware.responseTimeBefore request response
  requestBody <- Middleware.requestBodyJSONParser request response
  case requestBody of
    Left parseError -> handleJsonParseError parseError request response
    Right json -> do
      case Category.decodeCreateCategoryInput json of
        Left jsonDecodeError -> handleJsonDecodeError jsonDecodeError request response
        Right value -> do
          let category = Category.createCategory Nothing value.label
          let encodedCategory = Category.encodeCategory category
          responseBody <- Middleware.responseBodyJSONSerializer encodedCategory request response
          HTTPResponse.jsonContentTypeHeader request response
          HTTPResponse.created (Just responseBody) request response
  Middleware.responseTimeAfter before request response

listCategory :: Handler
listCategory request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

showCategory :: Handler
showCategory request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

updateCategory :: Handler
updateCategory request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

deleteCategory :: Handler
deleteCategory request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response
