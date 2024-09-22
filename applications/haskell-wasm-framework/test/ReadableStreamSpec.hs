{-# LANGUAGE OverloadedStrings #-}

module Streams.ReadableStreamSpec where

import Test.Hspec
import Streams.ReadableStream

spec :: Spec
spec = do
  describe "ReadableStream" $ do
    it "should create a new ReadableStream" $ do
      stream <- newReadableStream
      -- Add assertions as needed to verify behavior
      True `shouldBe` True
