module Domain.Todo where

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Lib.Array (with, without)
import Domain (Todo, Category, Tag)

createTodo :: String -> String -> Boolean -> Todo
createTodo id label completed =
  { id: id
  , label: label
  , completed: completed
  , category: Nothing
  , tags: []
  }

updateTodoLabel :: String -> Todo -> Todo
updateTodoLabel label todo =
  { id: todo.id
  , label: label
  , completed: todo.completed
  , category: todo.category
  , tags: todo.tags
  }

updateTodoCompleted :: Boolean -> Todo -> Todo
updateTodoCompleted completed todo =
  { id: todo.id
  , label: todo.label
  , completed: completed
  , category: todo.category
  , tags: todo.tags
  }

updateTodoCategory :: Maybe Category -> Todo -> Todo
updateTodoCategory category todo =
  { id: todo.id
  , label: todo.label
  , completed: todo.completed
  , tags: todo.tags
  , category: category
  }

addTagToTodo :: Tag -> Todo -> Todo
addTagToTodo tag todo =
  { id: todo.id
  , label: todo.label
  , completed: todo.completed
  , category: Nothing
  , tags: with tag todo.tags
  }

removeTagFromTodo :: Tag -> Todo -> Todo
removeTagFromTodo tag todo =
  { id: todo.id
  , label: todo.label
  , completed: todo.completed
  , category: Nothing
  , tags: without tag todo.tags
  }

encodeTodo :: Todo -> Json
encodeTodo = encodeJson

decodeTodo :: Json -> Either JsonDecodeError Todo
decodeTodo = decodeJson
