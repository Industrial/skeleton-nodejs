import { log } from '@code9/log'
import { assertIsNotUndefined } from '@code9/undefined'
import { createAsync } from '@solidjs/router'
import { type JSX } from 'solid-js'

import { NetWorthChart } from '~/components/NetWorthChart/NetWorthChart'
import { db } from '~/lib/db'
import { findWalletByAddress } from '~/lib/db/generated/queries'

const { WALLET_ADDRESS } = process.env

export default function Home(): JSX.Element {
  log.info({
    method: 'Home',
  })

  assertIsNotUndefined(WALLET_ADDRESS)

  const wallet = createAsync(async () =>
    await findWalletByAddress(db, {
      address: WALLET_ADDRESS,
    }))

  console.log('wallet', wallet())

  return (
    <main>
      <NetWorthChart />
    </main>
  )
}
