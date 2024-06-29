import { assertIsNotUndefined } from '@code9/undefined'
import { Title } from '@solidjs/meta'
import { createAsync } from '@solidjs/router'
import { type JSX } from 'solid-js'

import Counter from '~/components/Counter'
import { db } from '~/lib/db'
import { findWalletByAddress } from '~/lib/db/generated/queries'

const { WALLET_ADDRESS } = process.env

export default function Home(): JSX.Element {
  assertIsNotUndefined(WALLET_ADDRESS)

  const wallet = createAsync(async () =>
    await findWalletByAddress(db, {
      address: WALLET_ADDRESS,
    }))

  console.log('wallet', wallet())

  return (
    <main>
      <Title>Hello World</Title>
      <h1>Hello world!</h1>
      <Counter />
      <p>
        Visit{' '}
        <a href="https://start.solidjs.com" target="_blank">
          start.solidjs.com
        </a>{' '}
        to learn how to build SolidStart apps.
      </p>
      <pre>{JSON.stringify(wallet(), null, 2)}</pre>
      {/* <Show when={wallet()} fallback={<div>Loading...</div>}>
        <pre>{JSON.stringify(wallet, null, 2)}</pre>
      </Show> */}
    </main>
  )
}
