{-# LANGUAGE ForeignFunctionInterface #-}
module Streams.WritableStream where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim

foreign import javascript "new WritableStream()"
  js_newWritableStream :: IO JSVal

-- Additional Haskell-side functions and type definitions for WritableStream
newtype WritableStream = WritableStream { jsVal :: JSVal }

newWritableStream :: IO WritableStream
newWritableStream = WritableStream <$> js_newWritableStream
