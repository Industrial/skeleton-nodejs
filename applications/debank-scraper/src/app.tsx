import './app.css'

import { log } from '@code9/log'
import { MetaProvider, Title } from '@solidjs/meta'
import { Router, type RouteSectionProps } from '@solidjs/router'
import { FileRoutes } from '@solidjs/start/router'
import { type JSX, Suspense } from 'solid-js'

export const App = (props: RouteSectionProps): JSX.Element => {
  log.info({
    method: 'App',
  })

  return (
    <MetaProvider>
      <Title>Derp</Title>
      <a href="/">Index</a>
      <a href="/about">About</a>
      {/* eslint-disable-next-line react/destructuring-assignment */}
      <Suspense>{props.children}</Suspense>
    </MetaProvider>
  )
}

export default function AppContainer(): JSX.Element {
  log.info({
    method: 'AppContainer',
  })

  return (
    <Router root={App}>
      <FileRoutes />
    </Router>
  )
}
