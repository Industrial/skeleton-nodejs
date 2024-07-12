import { log } from '@code9/log'
import { assertIsUndefined } from '@code9/undefined'
import type { APIEvent } from '@solidjs/start/server'

import { db, e } from '~/lib/db'

export const GET = async ({ params }: APIEvent) => {
  log.info({
    method: 'net-worth',
  })

  const { walletAddress } = params
  assertIsUndefined(walletAddress)

  const value = await e.select(e.NetWorth, (netWorth) => {
    return {
      id: true,
      date: true,
      value: true,
      wallet: {
        id: true,
        address: true,
      },
      filter: e.op(netWorth.wallet.address, '=', walletAddress),
      order_by: {
        expression: netWorth.date,
        direction: e.ASC,
        empty: e.EMPTY_FIRST,
      },
    }
  }).run(db)

  return value
}
