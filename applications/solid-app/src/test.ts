import { readFile } from 'fs/promises'
import { WASI } from 'wasi'

const wasi = new WASI({
  args: [],
  env: {},
  preopens: {
    '/sandbox': './',
  },
  version: 'preview1',
})

const loadStreaming = async () => {
  // TODO: WebAssembly.instantiateStreaming is not supported in BunJS
  const __exports = {}
  const wasm_url = `${import.meta.dirname}/haskell/dist/MyLib.wasm`
  const js_url = `${import.meta.dirname}/haskell/dist/MyLib.js`
  const { instance } = await WebAssembly.instantiateStreaming(fetch(wasm_url), {
    ghc_wasm_jsffi: (await import(js_url)).default(__exports),
    wasi_snapshot_preview1: wasi.wasiImport,
  })
  Object.assign(__exports, instance.exports)
  wasi.initialize(__exports)
  return __exports
}

export type Exports = {
  hs_init: (argc: number, argv: number) => void
  fib: (n: number) => number
  fac: (n: number) => number
}

const loadCompiled = async <T>(): Promise<T> => {
  const wasmUrl = `${import.meta.dirname}/haskell/dist/MyLib.wasm`
  const jsUrl = `${import.meta.dirname}/haskell/dist/MyLib.js`
  const wasmBinary = await readFile(wasmUrl)
  const wasmModule = await WebAssembly.compile(wasmBinary)
  const __exports = {}
  const instance = await WebAssembly.instantiate(wasmModule, {
    ghc_wasm_jsffi: (await import(jsUrl)).default(__exports),
    wasi_snapshot_preview1: wasi.wasiImport,
  })
  wasi.start(instance)
  return instance.exports as T
}

const exports = await loadCompiled<Exports>()

exports.hs_init(0, 0)
console.log(exports.fib(12))
console.log(exports.fac(1))
