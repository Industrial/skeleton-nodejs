{-# LANGUAGE OverloadedStrings #-}

module Streams.WritableStreamSpec where

import Test.Hspec
import Streams.WritableStream

spec :: Spec
spec = do
  describe "WritableStream" $ do
    it "should create a new WritableStream" $ do
      stream <- newWritableStream
      -- Add Assertions as needed to verify behavior
      True `shouldBe` True
