import { HttpServerRequest, HttpServerResponse } from '@effect/platform'
import { Effect, pipe } from 'effect'
import type { ReactNode } from 'react'
import { renderToReadableStream } from 'react-dom/server'
import {
  concatenate,
  fromReactDomServerReadableStream,
  fromString,
} from 'src/stream.js'

export const ssr = (component: ReactNode, top: string, bottom: string) =>
  pipe(
    HttpServerRequest.HttpServerRequest,
    Effect.flatMap((req) =>
      pipe(
        Effect.tryPromise(() => renderToReadableStream(component)),
        Effect.map((middle) =>
          concatenate([
            fromString(top),
            fromReactDomServerReadableStream(middle),
            fromString(bottom),
          ]),
        ),
        Effect.map((body) => ({ req, body })),
      ),
    ),
    Effect.map(({ req, body }) =>
      HttpServerResponse.raw(body).pipe(
        HttpServerResponse.setHeader('Content-Type', 'text/html'),
      ),
    ),
  )
