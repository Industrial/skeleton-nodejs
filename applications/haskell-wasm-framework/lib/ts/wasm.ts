import { readFile } from "fs/promises"
import { wasi } from "./wasi"

export const loadCompiled = async <T>(
  wasmUrl: string,
  jsUrl: string,
): Promise<T> => {
  const wasmBinary = await readFile(wasmUrl)
  const wasmModule = await WebAssembly.compile(wasmBinary)

  const __exports = {}

  const instance = await WebAssembly.instantiate(wasmModule, {
    ghc_wasm_jsffi: (await import(jsUrl)).default(__exports),
    wasi_snapshot_preview1: wasi.wasiImport
  })

  wasi.start(instance)

  return instance.exports as T
}

export const loadStreaming = async () => {
  // TODO: WebAssembly.instantiateStreaming is not supported in BunJS
  const __exports = {}
  const wasm_url = `${import.meta.dirname}/haskell/dist/Main.wasm`
  const js_url = `${import.meta.dirname}/haskell/dist/Main.js`
  const { instance } = await WebAssembly.instantiateStreaming(
    fetch(wasm_url),
    {
      ghc_wasm_jsffi: (await import(js_url)).default(__exports),
      wasi_snapshot_preview1: wasi.wasiImport
    },
  )
  Object.assign(__exports, instance.exports)
  wasi.initialize(__exports)
  return __exports
}
