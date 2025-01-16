import './Counter.css'

import { type JSX, createSignal } from 'solid-js'

export default function Counter(): JSX.Element {
  const [count, setCount] = createSignal(0)
  return (
    <button
      type="button"
      class="increment"
      onClick={() => setCount(count() + 1)}
    >
      Clicks: {count()}
    </button>
  )
}
