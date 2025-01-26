// @refresh reload
import { StartServer, createHandler } from '@solidjs/start/server'
import { Effect, Runtime } from 'effect'

const myEffect = Effect.sync(() => {
  console.log('Running an effect on the server start')
})

export default createHandler(() => (
  <StartServer
    document={({ assets, children, scripts }) => (
      <html lang="en">
        <head>
          <meta charset="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <link rel="icon" href="/favicon.ico" />
          {assets}
        </head>
        <body>
          <div id="app">{children}</div>
          {scripts}
        </body>
      </html>
    )}
  />
))
