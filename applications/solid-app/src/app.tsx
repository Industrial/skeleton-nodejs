import './app.css'

import { MetaProvider, Title } from '@solidjs/meta'
import { Router, type RouteSectionProps } from '@solidjs/router'
import { FileRoutes } from '@solidjs/start/router'
import { type JSX, Suspense } from 'solid-js'

export const App = ({ children }: RouteSectionProps): JSX.Element =>
  (
    <MetaProvider>
      <Title>SolidStart - Basic</Title>
      <a href="/">Index</a>
      <a href="/about">About</a>
      <Suspense>{children}</Suspense>
    </MetaProvider>
  )

export default function AppContainer(): JSX.Element {
  return (
    <Router root={App}>
      <FileRoutes />
    </Router>
  )
}
