import './Counter.css'

import { createSignal, type JSX } from 'solid-js'

export default function Counter(): JSX.Element {
  const [count, setCount] = createSignal(0)
  return (
    <button class="increment" onClick={() => setCount(count() + 1)}>
      Clicks: {count()}
    </button>
  )
}
