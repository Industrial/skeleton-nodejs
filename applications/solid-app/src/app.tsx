import './app.css'

import { MetaProvider, Title } from '@solidjs/meta'
import { Router, type RouteSectionProps } from '@solidjs/router'
import { FileRoutes } from '@solidjs/start/router'
import { Suspense, type JSX } from 'solid-js'

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
