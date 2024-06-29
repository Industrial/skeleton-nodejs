// @refresh reload
import { log } from '@code9/log'
import { assertIsNotUndefined } from '@code9/undefined'
import nodeSchedule from 'node-schedule'

import { db } from './db'
import { findWalletByAddress } from './db/generated/queries'

const { WALLET_ADDRESS } = process.env
assertIsNotUndefined(WALLET_ADDRESS)

nodeSchedule.scheduleJob({ second: 0 }, async () => {
  log.info('Running importer...')

  const wallet = await findWalletByAddress(db, {
    address: WALLET_ADDRESS,
  })

  console.log('wallet123', wallet)
})
