module Lib.Middleware.Test where

import Prelude

import Effect.Aff (Aff, launchAff)
import Effect.Class.Console as Console
import Node.HTTP (Request, request)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

import Lib.Middleware (Middleware, runMiddlewareChain)

-- Sample request for testing
sampleRequest :: Request
sampleRequest = request "GET" "/" []

-- Mock response function
mockResponse :: String -> Request -> Aff Response
mockResponse result _ = do
  liftEffect $ Console.log result
  pure request -- returning the request just to simplify the response type

-- Test the middleware chain
spec :: Effect Unit
spec = launchAff $ describe "Middleware Chain Tests" do

  it "runs middleware and transforms context" do
    let
      middleware1 :: Middleware Int String
      middleware1 context req next = next (show context <> "a") req

    let
      middleware2 :: Middleware String String
      middleware2 context req next = next (context <> "b") req

    let middlewares = [ middleware1, middleware2 ]

    runMiddlewareChain middlewares 1 sampleRequest \finalContext req -> do
      finalContext `shouldEqual` "1ab"
      mockResponse finalContext req