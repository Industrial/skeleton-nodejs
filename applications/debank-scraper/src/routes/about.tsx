import { log } from '@code9/log'
import { Title } from '@solidjs/meta'
import type { JSX } from 'solid-js'

export default function Home(): JSX.Element {
  log.debug({
    method: 'About',
  })

  return (
    <main>
      <Title>About</Title>
      <h1>About</h1>
    </main>
  )
}
