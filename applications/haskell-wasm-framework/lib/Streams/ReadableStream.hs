{-# LANGUAGE ForeignFunctionInterface #-}
module Streams.ReadableStream where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim (JSVal, jsNull, jsUndefined)

foreign import javascript "new ReadableStream()"
  js_new :: IO JSVal

foreign import javascript "($1.read ? $1.read() : null)"
  js_read :: JSVal -> IO JSVal

foreign import javascript "($1.cancel ? $1.cancel($2) : null)"
  js_cancel :: JSVal -> JSVal -> IO JSVal

foreign import javascript "($1.pipeTo ? $1.pipeTo($2, $3) : null)"
  js_pipeTo :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "($1.pipeThrough ? $1.pipeThrough($2, $3) : null)"
  js_pipeThrough :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "($1.locked ? true : false)"
  js_locked :: JSVal -> IO Bool

foreign import javascript "($1.getReader ? $1.getReader() : null)"
  js_getReader :: JSVal -> IO JSVal

newtype ReadableStream = ReadableStream { jsVal :: JSVal }

new :: IO ReadableStream
new = ReadableStream <$> js_new

read :: ReadableStream -> IO JSVal
read (ReadableStream jsval) = js_read jsval

cancel :: ReadableStream -> JSVal -> IO JSVal
cancel (ReadableStream jsval) = js_cancel jsval

pipeTo :: ReadableStream -> JSVal -> JSVal -> IO JSVal
pipeTo (ReadableStream jsval) = js_pipeTo jsval

pipeThrough :: ReadableStream -> JSVal -> JSVal -> IO JSVal
pipeThrough (ReadableStream jsval) = js_pipeThrough jsval

locked :: ReadableStream -> IO Bool
locked (ReadableStream jsval) = js_locked jsval

getReader :: ReadableStream -> IO JSVal
getReader (ReadableStream jsval) = js_getReader jsval
