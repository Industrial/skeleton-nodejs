import { Title } from '@solidjs/meta'
import { createAsync } from '@solidjs/router'
import { type JSX, Show } from 'solid-js'

import Counter from '~/components/Counter'
import { db } from '~/lib/db'
import { findWalletByAddress } from '~/lib/db/generated/queries'

const WALLET_ADDRESS = '0x1a2b3c4d5e6f7g8h9i0j'

export default function Home(): JSX.Element {
  const wallet = createAsync(async () => {
    console.log('findWalletByAddress')
    const result = await findWalletByAddress(db, {
      address: WALLET_ADDRESS,
    })
    console.log('findWalletByAddress:wallet', result)
    return result
  })

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
      <Show when={wallet()} fallback={<div>Loading...</div>}>
        <pre>{JSON.stringify(wallet, null, 2)}</pre>
      </Show>
    </main>
  )
}
