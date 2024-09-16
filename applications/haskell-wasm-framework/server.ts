import { readFile } from 'node:fs/promises'

import { wasi } from './lib/ts/wasi'

const wasmUrl = `${import.meta.dirname}/dist/Main.wasm`
const jsUrl = `${import.meta.dirname}/dist/Main.js`

const wasmBinary = await readFile(wasmUrl)
const wasmModule = await WebAssembly.compile(wasmBinary)

const memory = new WebAssembly.Memory({
  initial: 1,
})

// eslint-disable-next-line no-underscore-dangle
const __exports = {
  test123: async (x: number) => {
    console.log('test123:env', x)
    return x + 1
  },
}
const ghcWASMFFI = (await import(jsUrl)).default(__exports)

const instance = await WebAssembly.instantiate(wasmModule, {
  // eslint-disable-next-line camelcase
  ghc_wasm_jsffi: ghcWASMFFI,
  // eslint-disable-next-line camelcase
  wasi_snapshot_preview1: wasi.wasiImport,
  env: {
    memory,
  },
})
Object.assign(__exports, instance.exports)

export type Exports = WebAssembly.Exports & {
  hs_init: (argc: number, argv: number) => void
  main: () => Promise<void>
}

const exports = instance.exports as Exports

console.log('exports', exports)

wasi.start(instance)
exports.hs_init(0, 0)
await exports.main()
