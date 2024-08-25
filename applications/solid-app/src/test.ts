import { readFileSync } from 'fs';
import { WASI } from 'wasi';

const wasi = new WASI({
  args: [],
  env: {},
  preopens: {
    "/sandbox": "./"
  },
  version: "preview1"
})

// Load the WebAssembly module
const wasmBinary = readFileSync(`${import.meta.dirname}/haskell/dist/MyLib.wasm`);
const wasmModule = await WebAssembly.compile(wasmBinary);
const instance = await WebAssembly.instantiate(wasmModule, {
  wasi_snapshot_preview1: wasi.wasiImport
})

// Start the WASI instance
wasi.start(instance)

const exports = instance.exports as {
  hs_init: (argc: number, argv: number) => void
  fib: (n: number) => number
  // fac: (n: number) => number
}

// This function is a part of GHC's RTS API. It must be called before any other exported Haskell functions are called.
exports.hs_init(0, 0)

console.log(exports.fib(12))
// console.log(exports.fac(12))
