{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim

foreign import javascript "return await __exports.readdir($1)"
  js_readdir :: String -> IO JSVal

foreign import javascript "return await __exports.test123($1)"
  js_test123 :: Int -> IO Int

foreign import javascript "return String($1);"
  js_toString :: JSVal -> IO JSString

foreign import javascript "console.log($1)"
  js_consoleLogString :: JSString -> IO ()

foreign export javascript
  main :: IO ()

main = do
  result <- js_readdir "."
  strResult <- js_toString result
  js_consoleLogString strResult -- directly log the converted string to the console
  -- putStrLn $ "Haskell result: " ++ show result
