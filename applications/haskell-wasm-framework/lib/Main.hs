{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim

foreign import javascript "return await __exports.test123($1)"
  js_test123 :: Int -> IO Int

foreign import javascript "console.log($1)"
  js_consoleLogString :: JSString -> IO ()

foreign export javascript
  main :: IO ()

main = do
  putStrLn "Running simple test from Haskell!"
  js_consoleLogString $ toJSString "Hello from Haskell!"
  result <- js_test123 123
  putStrLn $ "Haskell result: " ++ show result
