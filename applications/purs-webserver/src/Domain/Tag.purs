module Domain.Tag where

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Domain (Tag)

createTag :: String -> String -> Tag
createTag id label =
  { id: id
  , label: label
  }

updateTagLabel :: String -> Tag -> Tag
updateTagLabel label tag =
  { id: tag.id
  , label: label
  }

encodeTag :: Tag -> Json
encodeTag = encodeJson

decodeTag :: Json -> Either JsonDecodeError Tag
decodeTag = decodeJson
