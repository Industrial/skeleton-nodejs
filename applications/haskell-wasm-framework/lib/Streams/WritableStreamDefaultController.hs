{-# LANGUAGE ForeignFunctionInterface #-}
module Streams.WritableStreamDefaultController where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim

foreign import javascript "new WritableStreamDefaultController()"
  js_newWritableStreamDefaultController :: IO JSVal

-- Additional Haskell-side functions and type definitions for WritableStreamDefaultController
newtype WritableStreamDefaultController = WritableStreamDefaultController { jsVal :: JSVal }

newWritableStreamDefaultController :: IO WritableStreamDefaultController
newWritableStreamDefaultController = WritableStreamDefaultController <$> js_newWritableStreamDefaultController
