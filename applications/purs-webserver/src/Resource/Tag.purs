module Resource.Tag where

import Prelude

import Data.Maybe (Maybe(..))
import Lib (Handler)
import Lib.HTTPResponse as HTTPResponse
import Lib.Middleware as Middleware

createTag :: Handler
createTag request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

listTag :: Handler
listTag request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

showTag :: Handler
showTag request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

updateTag :: Handler
updateTag request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response

deleteTag :: Handler
deleteTag request response = do
  before <- Middleware.responseTimeBefore request response
  HTTPResponse.jsonContentTypeHeader request response
  HTTPResponse.badRequest Nothing request response
  Middleware.responseTimeAfter before request response
