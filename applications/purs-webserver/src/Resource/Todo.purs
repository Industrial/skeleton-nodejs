module Resource.Todo where

import Prelude

import Data.Maybe (Maybe(..))
import Lib (Handler)
import Lib.HTTPResponse as HTTPResponse
import Lib.Middleware as Middleware

createTodo :: Handler
createTodo request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

listTodo :: Handler
listTodo request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

showTodo :: Handler
showTodo request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

updateTodo :: Handler
updateTodo request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

deleteTodo :: Handler
deleteTodo request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response
