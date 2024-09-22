{-# LANGUAGE ForeignFunctionInterface #-}
module Streams.ReadableStreamDefaultController where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim

foreign import javascript "new ReadableStreamDefaultController()"
  js_newReadableStreamDefaultController :: IO JSVal

-- Additional Haskell-side functions and type definitions for ReadableStreamDefaultController
newtype ReadableStreamDefaultController = ReadableStreamDefaultController { jsVal :: JSVal }

newReadableStreamDefaultController :: IO ReadableStreamDefaultController
newReadableStreamDefaultController = ReadableStreamDefaultController <$> js_newReadableStreamDefaultController
