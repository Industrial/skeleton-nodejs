import { HttpRouter, HttpServer } from '@effect/platform'
import { BunHttpServer, BunRuntime } from '@effect/platform-bun'
import { Layer } from 'effect'
import React from 'react'
import { App } from './components/App.js'
import { rpc } from './routes/rpc.js'
import { ssr } from './routes/ssr.js'

const top = `<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Document</title>
    <script type="module" src="/src/entry-client.tsx"></script>
  </head>
  <body>
    <div id="root">
      `

const bottom = `
    </div>
  </body>
</html>
`

const HttpLive = HttpRouter.empty.pipe(
  HttpRouter.get('/', ssr(<App />, top, bottom)),
  HttpRouter.post('/rpc', rpc()),
  HttpServer.serve(),
  HttpServer.withLogAddress,
  Layer.provide(
    BunHttpServer.layer({
      port: 3000,
    }),
  ),
)

BunRuntime.runMain(Layer.launch(HttpLive))
