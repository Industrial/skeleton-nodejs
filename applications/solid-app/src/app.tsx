import './app.css'

import { MetaProvider, Title } from '@solidjs/meta'
import { Router, type RouteSectionProps } from '@solidjs/router'
import { FileRoutes } from '@solidjs/start/router'
import { readFileSync } from 'fs'
import { Suspense, type JSX } from 'solid-js'
import { WASI } from 'wasi'

export const App = (props: RouteSectionProps): JSX.Element =>
  (
    <MetaProvider>
      <Title>Derp</Title>
      <a href="/">Index</a>
      <a href="/about">About</a>
      {/* eslint-disable-next-line react/destructuring-assignment */}
      <Suspense>{props.children}</Suspense>
    </MetaProvider>
  )

export default function AppContainer(): JSX.Element {
  return (
    <Router root={App}>
      <FileRoutes />
    </Router>
  )
}

const wasi = new WASI({
  args: [],
  env: {},
  preopens: {
    "/sandbox": "./"
  },
  version: "preview1"
})

// Load the WebAssembly module
const wasmBinary = readFileSync("haskell/dist/Hello.wasm");
const wasmModule = await WebAssembly.compile(wasmBinary);
const instance = await WebAssembly.instantiate(wasmModule, {
  wasi_snapshot_preview1: wasi.wasiImport
})

// Start the WASI instance
wasi.start(instance)

console.log('exports', instance.exports)

// This function is a part of GHC's RTS API. It must be called before any other exported Haskell functions are called.
instance.exports.hs_init(0, 0)

console.log(instance.exports.fib(10))
