{-# LANGUAGE ForeignFunctionInterface #-}

module MyLib where

import Distribution.Compiler (CompilerFlavor (HBC))
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (FunPtr, castFunPtr)
import GHC.JS.Foreign.Callback (Callback, syncCallback1)
import GHC.Wasm.Prim

foreign import javascript "((f) => { f('Example!'); })"
  callback_example :: Callback (JSVal -> IO ()) -> IO ()

printJSValAsString :: JSVal -> IO ()
printJSValAsString = putStrLn . fromJSString

type HTTPServer = IO ()

type HTTPServerPort = Int

type HTTPRequestHandler = CString -> IO CString

type StartHTTPServer = HTTPServerPort -> FunPtr HTTPRequestHandler -> IO ()

foreign import javascript "startHTTPServer($1, $2)"
  js_startHTTPServer :: HTTPServerPort -> FunPtr HTTPRequestHandler -> IO ()

foreign import javascript "((x) => test123(x))"
  js_test123 :: Int -> IO Int

handleRequest :: CString -> IO CString
handleRequest cpath = do
  path <- peekCString cpath
  let response = case path of
        "/hello" -> "there"
        _ -> "Unknown Route"
  newCString response

foreign import ccall "wrapper"
  mkHandler :: HTTPRequestHandler -> IO (FunPtr HTTPRequestHandler)

foreign export ccall main :: IO ()

main :: IO ()
main = do
  printJS <- syncCallback1 ThrowWouldBlock printJSValAsString
  callback_example printJS
  releaseCallback printJS

-- result <- js_test123 123
-- print result

-- handlerFunPtr <- mkHandler handleRequest
-- js_startHTTPServer 8080 handlerFunPtr
-- print $ js_test123 123
