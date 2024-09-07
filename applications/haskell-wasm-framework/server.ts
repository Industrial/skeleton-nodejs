// import { readFile } from 'fs/promises'
// import { wasi } from './lib/ts/wasi'

// const wasmUrl = `${import.meta.dirname}/dist/Main.wasm`
// const jsUrl = `${import.meta.dirname}/dist/Main.js`

// const wasmBinary = await readFile(wasmUrl)
// const wasmModule = await WebAssembly.compile(wasmBinary)

// const __exports = {}

// const ghcWASMFFI = (await import(jsUrl)).default(__exports)

// const instance = await WebAssembly.instantiate(wasmModule, {
//   ghc_wasm_jsffi: ghcWASMFFI,
//   wasi_snapshot_preview1: wasi.wasiImport,
//   env: {
//     test123: async (x: number) => {
//       console.log('test123:env', x)
//       return x + 1
//     },
//   },
// })

// wasi.start(instance)

// export interface Exports extends WebAssembly.Exports {
//   hs_init: (argc: number, argv: number) => void
//   main: () => Promise<void>
// }

// const exports = instance.exports as Exports
// exports.hs_init(0, 0)
// await exports.main()

import { readFile } from 'fs/promises'
import { wasi } from './lib/ts/wasi'

const wasmUrl = `${import.meta.dirname}/dist/Main.wasm`
const jsUrl = `${import.meta.dirname}/dist/Main.js`

const wasmBinary = await readFile(wasmUrl)
const wasmModule = await WebAssembly.compile(wasmBinary)

const __exports = {}

const ghcWASMFFI = (await import(jsUrl)).default(__exports)

const instance = await WebAssembly.instantiate(wasmModule, {
  ghc_wasm_jsffi: ghcWASMFFI,
  wasi_snapshot_preview1: wasi.wasiImport,
  env: {
    test123: async (x: number) => {
      console.log('test123:env', x)
      return x + 1
    },
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
