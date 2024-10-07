module Main where

import Prelude

import Data.Tuple.Nested (tuple4)
import Effect (Effect)
import Lib.Application as Application
import Lib.Middleware as Middleware
import Resource.Category as CategoryResource
import Resource.Tag as TagResource
import Resource.Todo as TodoResource

main :: Effect Unit
main = do
  Application.listen 3000 "127.0.0.1"
    [ tuple4 "POST" "/category" [ Middleware.responseTime, Middleware.requestBodyParser, Middleware.requestBodyJSONParser ] CategoryResource.createCategory
    , tuple4 "GET" "/category" [ Middleware.responseTime ] CategoryResource.listCategory
    , tuple4 "GET" "/category/:id" [ Middleware.responseTime ] CategoryResource.showCategory
    , tuple4 "PUT" "/category/:id" [ Middleware.responseTime, Middleware.requestBodyJSONParser ] CategoryResource.updateCategory
    , tuple4 "DELETE" "/category/:id" [ Middleware.responseTime ] CategoryResource.deleteCategory

    , tuple4 "POST" "/tag" [ Middleware.responseTime, Middleware.requestBodyJSONParser ] TagResource.createTag
    , tuple4 "GET" "/tag" [ Middleware.responseTime ] TagResource.listTag
    , tuple4 "GET" "/tag/:id" [ Middleware.responseTime ] TagResource.showTag
    , tuple4 "PUT" "/tag/:id" [ Middleware.responseTime, Middleware.requestBodyJSONParser ] TagResource.updateTag
    , tuple4 "DELETE" "/tag/:id" [ Middleware.responseTime ] TagResource.deleteTag

    , tuple4 "POST" "/todo" [ Middleware.responseTime, Middleware.requestBodyJSONParser ] TodoResource.createTodo
    , tuple4 "GET" "/todo" [ Middleware.responseTime ] TodoResource.listTodo
    , tuple4 "GET" "/todo/:id" [ Middleware.responseTime ] TodoResource.showTodo
    , tuple4 "PUT" "/todo/:id" [ Middleware.responseTime, Middleware.requestBodyJSONParser ] TodoResource.updateTodo
    , tuple4 "DELETE" "/todo/:id" [ Middleware.responseTime ] TodoResource.deleteTodo
    ]