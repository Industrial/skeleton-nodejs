import {
  HttpRouter,
  HttpServer,
  HttpServerRequest,
  HttpServerResponse,
} from '@effect/platform'
import { BunHttpServer, BunRuntime } from '@effect/platform-bun'
import { toHttpApp } from '@effect/rpc-http/HttpRpcRouter'
import { Effect, Layer } from 'effect'
// import type { Route } from '@effect/platform/HttpRouter'
// import * as React from 'react'
// import { renderToPipeableStream } from 'react-dom/server'
// import { App } from './pages/App.js'
import { appRouter } from './router.js'

// const { pipe, abort } = renderToPipeableStream(<App />)

// const renderHandler: Route.Handler<never, string> = () => {
//   return Effect.succeed('Hello, world!')
// }

const HttpLive = HttpRouter.empty.pipe(
  HttpRouter.get(
    '/',
    Effect.map(HttpServerRequest.HttpServerRequest, (req) =>
      HttpServerResponse.text(req.url),
    ),
  ),
  HttpRouter.post('/rpc', toHttpApp(appRouter)),
  HttpServer.serve(),
  HttpServer.withLogAddress,
  Layer.provide(
    BunHttpServer.layer({
      port: 3000,
    }),
  ),
)

BunRuntime.runMain(Layer.launch(HttpLive))
