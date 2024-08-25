module MyLib (fib) where

import GHC.Wasm.Prim

foreign import javascript unsafe "console.log($1)"
  js_print :: JSString -> IO ()

-- foreign import javascript unsafe "typeof $1 === 'object'"
--   js_is_obj :: JSVal -> Bool

-- foreign import javascript unsafe "let acc = 1; for (let i = 1; i <= $1; ++i) acc *= i; return acc;"
--   js_fac :: Word -> Word

-- foreign export ccall fac :: Word -> Word

-- fac = js_fac

foreign export ccall fib :: Int -> Int

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
