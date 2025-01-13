import { Title } from '@solidjs/meta'
import type { JSX } from 'solid-js'

import Counter from '../components/Counter'

export default function Home(): JSX.Element {
  return (
    <main>
      <Title>Hello World</Title>
      <h1>Hello world!</h1>
      <Counter />
      <p>
        Visit{' '}
        <a href="https://start.solidjs.com" target="_blank" rel="noreferrer">
          start.solidjs.com
        </a>{' '}
        to learn how to build SolidStart apps.
      </p>
    </main>
  )
}
