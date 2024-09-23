{-# LANGUAGE OverloadedStrings #-}

module Streams.ReadableStreamSpec where

import Test.Hspec
import Streams.ReadableStream
import GHC.Wasm.Prim (JSVal, jsNull, jsUndefined)

spec :: Spec
spec = do
  describe "ReadableStream" $ do
    describe "new" $ do
      it "should create a new ReadableStream" $ do
        stream <- new
        (jsVal stream /= jsNull) `shouldBe` True

    -- describe "read" $ do
    --   describe "when the stream is readable" $ do
    --     it "should read from the ReadableStream" $ do
    --       stream <- new
    --       result <- read stream
    --       (result /= jsNull) `shouldBe` True

    --   -- Add more contexts as necessary, simulating non-readable conditions

    -- describe "cancel" $ do
    --   describe "with a reason" $ do
    --     it "should cancel the ReadableStream" $ do
    --       stream <- new
    --       reason <- pure jsUndefined -- provide a real reason if applicable
    --       result <- cancel stream reason
    --       (result /= jsNull) `shouldBe` True

    --     -- Add more contexts if cancel can be called without a reason
    --     -- describe "without a reason" ...

    -- describe "pipeTo" $ do
    --   describe "to a valid destination stream" $ do
    --     it "should pipe to another stream from the ReadableStream" $ do
    --       streamSrc <- new
    --       streamDest <- new -- Represents a writable stream in real tests
    --       options <- pure jsUndefined -- Placeholder for pipe options
    --       result <- pipeTo streamSrc (jsVal streamDest) options
    --       (result /= jsNull) `shouldBe` True

    --   -- Add more contexts for invalid destination, options, etc.

    -- describe "pipeThrough" $ do
    --   describe "with a valid transform stream" $ do
    --     it "should pipe through a transform stream from the ReadableStream" $ do
    --       streamSrc <- new
    --       transformStream <- new -- Represents a transform stream in real tests
    --       options <- pure jsUndefined -- Placeholder for pipe options
    --       result <- pipeThrough streamSrc (jsVal transformStream) options
    --       (result /= jsNull) `shouldBe` True

    --   -- Add more contexts for invalid transform, options, etc.

    -- describe "locked" $ do
    --   describe "when the stream is newly created" $ do
    --     it "should not be locked" $ do
    --       stream <- new
    --       isLocked <- locked stream
    --       isLocked `shouldBe` False

    --   -- Add more contexts to test a locked stream if possible

    -- describe "getReader" $ do
    --   describe "when getting a reader" $ do
    --     it "should get a reader from the ReadableStream" $ do
    --       stream <- new
    --       reader <- getReader stream
    --       (reader /= jsNull) `shouldBe` True

    --   -- Add more contexts to test behavior with multiple readers or locked streams
