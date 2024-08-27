import { loadCompiled } from "./lib/ts/wasm.ts"

const wasmUrl = `${import.meta.dirname}/dist/MyLib.wasm`
const jsUrl = `${import.meta.dirname}/dist/MyLib.js`

export type Exports = {
  hs_init: (argc: number, argv: number) => void
  fib: (n: number) => number
  fac: (n: number) => number
}

export const exports = await loadCompiled<Exports>(wasmUrl, jsUrl)

exports.hs_init(0, 0)
console.log(exports.fib(12))
console.log(exports.fac(1))
