module Domain where

import Data.Maybe (Maybe)

type Category =
  { id :: String
  , label :: String
  }

type CreateCategoryInput =
  { label :: String }

type Tag =
  { id :: String
  , label :: String
  }

type CreateTagInput =
  { label :: String }

type Todo =
  { id :: String
  , label :: String
  , completed :: Boolean
  , category :: Maybe Category
  , tags :: Array Tag
  }

type CreateTodoInput =
  { label :: String }