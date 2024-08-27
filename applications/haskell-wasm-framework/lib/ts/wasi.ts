import { WASI } from 'wasi'

export const wasi = new WASI({
  args: [],
  env: {},
  preopens: {},
  version: "preview1"
})
