{-# LANGUAGE ForeignFunctionInterface #-}
module Streams.ReadableStream where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim

foreign import javascript "new ReadableStream()" 
  js_newReadableStream :: IO JSVal

-- Additional Haskell-side functions and type definitions for ReadableStream
newtype ReadableStream = ReadableStream {jsVal :: JSVal}

newReadableStream :: IO ReadableStream
newReadableStream = ReadableStream <$> js_newReadableStream
