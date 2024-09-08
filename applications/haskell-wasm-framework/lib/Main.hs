{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CString, withCString)
import GHC.Wasm.Prim (JSString, JSVal)

-- foreign import javascript safe "return await $1();"
--   callInput :: JSVal -> IO JSString
-- foreign import javascript safe "await $1($2);"
--   callOutput :: JSVal -> JSString -> IO ()

-- foreign import javascript "(async (x) => await env.test123(x))"
foreign import javascript "return await env.test123($1)"
  js_test123 :: Int -> IO Int

foreign import javascript "console.log($1)"
  js_consoleLog :: Int -> IO ()

foreign import javascript "console.log($1)"
  js_consoleLogString :: CString -> IO ()

foreign export ccall main :: IO ()

main :: IO ()
main = do
  putStrLn "Running simple test from Haskell!"
  withCString "env" js_consoleLogString
  js_consoleLog 42
  -- putStrLn $ "Test213 Result: " ++ show (js_test123 123)
  result <- js_test123 123
  putStrLn $ "Haskell result: " ++ show result
