import Distribution.Simple
import Distribution.Simple.Setup (buildVerbosity)
import Distribution.Verbosity (normal, verbose)
import System.Process (callProcess)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postBuild = \_ _ _ _ -> do
          let verbosity = chooseVerbosity (buildVerbosity undefined) -- Adjust verbosity as needed
          putStrLn "> bin/build-haskell"
          callProcess
            "wasm32-wasi-ghc"
            [ "-v",
              "lib/Main.hs",
              "-o",
              "dist/Main.wasm",
              "-no-hs-main",
              "-ddump-if-trace",
              "-optl-mexec-model=reactor",
              "-optl-Wl,--export=hs_init,--export=main"
            ]
      }

chooseVerbosity :: Maybe String -> String
chooseVerbosity (Just "normal") = normal
chooseVerbosity (Just "verbose") = verbose
chooseVerbosity _ = normal
