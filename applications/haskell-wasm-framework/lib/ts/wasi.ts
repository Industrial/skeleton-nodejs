import { WASI } from 'node:wasi'

export const wasi = new WASI({
  args: [],
  env: {},
  preopens: {},
  version: 'preview1',
})
