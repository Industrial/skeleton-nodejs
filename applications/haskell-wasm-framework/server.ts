import { readFile } from 'fs/promises'
import { WASI } from 'wasi'
import { startHTTPServer } from './lib/ts/http-server.ts'

const wasmUrl = `${import.meta.dirname}/dist/MyLib.wasm`
const jsUrl = `${import.meta.dirname}/dist/MyLib.js`

const wasmBinary = await readFile(wasmUrl)
const wasmModule = await WebAssembly.compile(wasmBinary)

const wasi = new WASI({
  args: [],
  env: {},
  preopens: {},
  version: "preview1"
})

const __exports = {
  startHTTPServer,
  test123: (x: number) => {
    console.log('test123:__exports', x)
    return x + 1
  },
}

const ghcWASMFFI = (await import(jsUrl)).default(__exports)

const instance = await WebAssembly.instantiate(wasmModule, {
  ghc_wasm_jsffi: ghcWASMFFI,
  wasi_snapshot_preview1: wasi.wasiImport,
  env: {
    test123: (x: number) => {
      console.log('test123:env', x)
      return x + 1
    },
    memory: new WebAssembly.Memory({
      initial: 256
    }),
  },
})

wasi.start(instance)

export interface Exports extends WebAssembly.Exports {
  hs_init: (argc: number, argv: number) => void
  main: () => Promise<void>
}

const exports = instance.exports as Exports
exports.hs_init(0, 0)
await exports.main()
