{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C (CString, withCString)

foreign import javascript "env.test123"
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
  result <- js_test123 123
  putStrLn $ "Haskell result: " ++ show result
