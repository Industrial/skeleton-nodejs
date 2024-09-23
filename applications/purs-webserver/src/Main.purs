module Main where

import Prelude

import Data.Tuple.Nested (tuple3)
import Effect (Effect)
import Lib.Application as Application
import Resource.Category as CategoryResource
import Resource.Tag as TagResource
import Resource.Todo as TodoResource

main :: Effect Unit
main = do
  Application.listen 3000 "127.0.0.1"
    [ tuple3 "POST" "/category" CategoryResource.createCategory
    , tuple3 "GET" "/category" CategoryResource.listCategory
    , tuple3 "GET" "/category/:id" CategoryResource.showCategory
    , tuple3 "PUT" "/category/:id" CategoryResource.updateCategory
    , tuple3 "DELETE" "/category/:id" CategoryResource.deleteCategory

    , tuple3 "POST" "/tag" TagResource.createTag
    , tuple3 "GET" "/tag" TagResource.listTag
    , tuple3 "GET" "/tag/:id" TagResource.showTag
    , tuple3 "PUT" "/tag/:id" TagResource.updateTag
    , tuple3 "DELETE" "/tag/:id" TagResource.deleteTag

    , tuple3 "POST" "/todo" TodoResource.createTodo
    , tuple3 "GET" "/todo" TodoResource.listTodo
    , tuple3 "GET" "/todo/:id" TodoResource.showTodo
    , tuple3 "PUT" "/todo/:id" TodoResource.updateTodo
    , tuple3 "DELETE" "/todo/:id" TodoResource.deleteTodo
    ]
