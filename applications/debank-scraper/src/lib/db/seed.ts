import assert from 'assert'

import { db } from '~/lib/db'

import { addWallet, findWalletByAddress } from './generated/queries'

const { WALLET_ADDRESS } = process.env
assert(WALLET_ADDRESS, 'WALLET_ADDRESS is not set')

const existingWallet = await findWalletByAddress(db, {
  address: WALLET_ADDRESS,
})
console.log('existingWallet', existingWallet)
if (!existingWallet) {
  console.log('Adding wallet')
  const newWallet = await addWallet(db, {
    address: WALLET_ADDRESS,
  })
  console.log('newWallet', newWallet)
}
