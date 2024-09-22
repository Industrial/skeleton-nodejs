{-# LANGUAGE OverloadedStrings #-}

module MainSpec where

import Test.Hspec
import Streams.ReadableStreamSpec
import Streams.WritableStreamSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Streams API Tests" $ do
    Streams.ReadableStreamSpec.spec
    Streams.WritableStreamSpec.spec
