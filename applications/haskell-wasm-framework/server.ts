// 1. My runtime is BunJS, which implements Node.js APIs and WASI.
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

// 2. I don't know where these exports are for, but according to the generated
//    jsffi .js file it contains:
//    - .rts_freeStablePtr(sp)
//    - .rts_schedulerLoop
//    - .rts_promiseResolveInt
//    - .rts_promiseResolveUnit
//    - .rts_promiseReject
//    - .memory
const __exports = {}

const instance = await WebAssembly.instantiate(wasmModule, {
  ghc_wasm_jsffi: (await import(jsUrl)).default(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
  env: {
    // 3. This is the function that I want to expose to haskell so that I can
    //    call it inside haskell code.
    test123: (x: number) => {
      console.log('test123:env', x);
      return x + 1;
    },
  },
})

export interface Exports extends WebAssembly.Exports {
  hs_init: (argc: number, argv: number) => void;
  main: () => Promise<void>;
}

const exports = instance.exports as Exports

wasi.start(instance)
exports.hs_init(0, 0);
await exports.main();
