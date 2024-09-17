import { wasi } from './lib/ts/wasi'
import { loadCompiled } from './lib/ts/wasm'

const wasmUrl = `${import.meta.dirname}/dist/Main.wasm`
const jsUrl = `${import.meta.dirname}/dist/Main.js`

export type Exports = WebAssembly.Exports & {
  hs_init: (argc: number, argv: number) => void
  main: () => Promise<void>
}

const instance = await loadCompiled<Exports>(wasmUrl, jsUrl, {
  test123: async (x: number) => {
    console.log('test123:env', x)
    return x + 1
  },
})

wasi.start(instance)
instance.exports.hs_init(0, 0)
await instance.exports.main()
