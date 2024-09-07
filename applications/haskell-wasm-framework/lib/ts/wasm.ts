import { readFile } from "fs/promises"
import { wasi } from "./wasi"

export interface InstanceWithExports<T extends Bun.WebAssembly.Exports> extends Bun.WebAssembly.Instance {
  exports: T
}

export const loadCompiled = async <T extends Bun.WebAssembly.Exports>(
  wasmUrl: string,
  jsUrl: string,
  env: Bun.WebAssembly.ModuleImports,
): Promise<InstanceWithExports<T>> => {
  const wasmBinary = await readFile(wasmUrl)
  const wasmModule = await WebAssembly.compile(wasmBinary)

  const __exports = {}

  const instance = await WebAssembly.instantiate(wasmModule, {
    ghc_wasm_jsffi: (await import(jsUrl)).default(__exports),
    wasi_snapshot_preview1: wasi.wasiImport,
    env,
  })

  wasi.start(instance)

  return {
    ...instance,
    exports: instance.exports as T,
  }
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
