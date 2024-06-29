import assert from 'assert'

import { db } from '~/lib/db'

import { addWallet } from './generated/queries'

const { WALLET_ADDRESS } = process.env
assert(WALLET_ADDRESS, 'WALLET_ADDRESS is not set')

await addWallet(db, {
  address: WALLET_ADDRESS,
})
