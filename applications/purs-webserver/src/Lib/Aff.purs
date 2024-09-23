module Lib.Aff where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)

affVersion :: forall a. ((Maybe Error -> Effect Unit) -> Effect a) -> Aff Unit
affVersion f =
  makeAff \callback -> do
    _ <- f \error -> do
      case error of
        Nothing -> do
          callback $ Right unit
        Just e -> do
          callback $ Left e
    mempty
