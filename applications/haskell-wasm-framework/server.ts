import { readFile } from 'node:fs/promises';
import { WASI } from 'wasi';

export const wasi = new WASI({
  args: [],
  env: {},
  preopens: {},
  version: "preview1"
})

const wasmUrl = `${import.meta.dirname}/dist/Main.wasm`;
const jsUrl = `${import.meta.dirname}/dist/Main.js`;

const wasmBinary = await readFile(wasmUrl)
const wasmModule = await WebAssembly.compile(wasmBinary)

const memory = new WebAssembly.Memory({
  initial: 1
})

const __exports = {
  test123: async (x: number) => {
    console.log('test123:env', x)
    return x + 1
  },
}
const ghc_wasm_jsffi = (await import(jsUrl)).default(__exports)

const instance = await WebAssembly.instantiate(wasmModule, {
  ghc_wasm_jsffi,
  wasi_snapshot_preview1: wasi.wasiImport,
  env: {
    memory,
  },
})
Object.assign(__exports, instance.exports)

export interface Exports extends WebAssembly.Exports {
  hs_init: (argc: number, argv: number) => void
  main: () => Promise<void>
}

const exports = instance.exports as Exports

console.log('exports', exports)

wasi.start(instance)
exports.hs_init(0, 0)
await exports.main()
