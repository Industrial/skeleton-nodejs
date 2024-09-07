{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

-- import Distribution.Compiler (CompilerFlavor (HBC))
-- import Foreign.C.String (CString, newCString, peekCString)
-- import Foreign.Ptr (FunPtr, castFunPtr)
-- import GHC.JS.Foreign.Callback (Callback, syncCallback1)
-- import GHC.Wasm.Prim
import Foreign.C.Types (CInt)

-- foreign import javascript "((f) => { f('Example!'); })"
--   callback_example :: Callback (JSVal -> IO ()) -> IO ()
-- printJSValAsString :: JSVal -> IO ()
-- printJSValAsString = putStrLn . fromJSString
-- type HTTPServer = IO ()
-- type HTTPServerPort = Int
-- type HTTPRequestHandler = CString -> IO CString
-- type StartHTTPServer = HTTPServerPort -> FunPtr HTTPRequestHandler -> IO ()
-- foreign import javascript "startHTTPServer($1, $2)"
--   js_startHTTPServer :: HTTPServerPort -> FunPtr HTTPRequestHandler -> IO ()
-- handleRequest :: CString -> IO CString
-- handleRequest cpath = do
--   path <- peekCString cpath
--   let response = case path of
--         "/hello" -> "there"
--         _ -> "Unknown Route"
--   newCString response
-- foreign import ccall "wrapper"
--   mkHandler :: HTTPRequestHandler -> IO (FunPtr HTTPRequestHandler)

-- foreign import javascript "env.test123"
foreign import javascript "console.log($1)"
  js_test123 :: Int -> IO Int

foreign export ccall main :: IO ()

main :: IO ()
main = do
  result <- js_test123 123
  print result
