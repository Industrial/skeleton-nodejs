{-# LANGUAGE ForeignFunctionInterface #-}
module Streams.WritableStreamDefaultWriter where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim

foreign import javascript "new WritableStreamDefaultWriter($1)"
  js_newWritableStreamDefaultWriter :: JSVal -> IO JSVal

-- Additional Haskell-side functions and type definitions for WritableStreamDefaultWriter
newtype WritableStreamDefaultWriter = WritableStreamDefaultWriter { jsVal :: JSVal }

newWritableStreamDefaultWriter :: WritableStream -> IO WritableStreamDefaultWriter
newWritableStreamDefaultWriter (WritableStream ws) = WritableStreamDefaultWriter <$> js_newWritableStreamDefaultWriter ws
