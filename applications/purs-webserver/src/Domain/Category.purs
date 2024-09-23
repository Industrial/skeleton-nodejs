module Domain.Category where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Domain (Category, CreateCategoryInput)
import Lib.Identifier (randomUUID)

createCategory :: Maybe String -> String -> Category
createCategory id label =
  case id of
    Just justId ->
      { id: justId
      , label: label
      }
    Nothing ->
      { id: randomUUID unit
      , label: label
      }

updateCategoryLabel :: String -> Category -> Category
updateCategoryLabel label category =
  { id: category.id
  , label: label
  }

encodeCategory :: Category -> Json
encodeCategory = encodeJson

decodeCategory :: Json -> Either JsonDecodeError Category
decodeCategory = decodeJson

encodeCreateCategoryInput :: CreateCategoryInput -> Json
encodeCreateCategoryInput = encodeJson

decodeCreateCategoryInput :: Json -> Either JsonDecodeError CreateCategoryInput
decodeCreateCategoryInput = decodeJson
