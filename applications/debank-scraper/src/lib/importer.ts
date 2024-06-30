import { log } from '@code9/log'
import { isNull } from '@code9/null'
import { assertIsNotUndefined } from '@code9/undefined'
import { Effect as Fx, Schedule } from 'effect'
import { chromium } from 'playwright'

import { db } from '~/lib/db'
import { addNetWorth, findWalletByAddress } from '~/lib/db/generated/queries'

const { WALLET_ADDRESS } = process.env
assertIsNotUndefined(WALLET_ADDRESS)

const browser = await chromium.launch()
const context = await browser.newContext()
const page = await context.newPage()

log.info('Fetching wallet...')
const wallet = await findWalletByAddress(db, {
  address: WALLET_ADDRESS,
})
if (isNull(wallet)) {
  throw new Error('Wallet not found')
}
log.info('wallet', wallet.id)

const task = async () => {
  try {
    log.info('Navigating to profile...')
    await page.goto(`https://debank.com/profile/${WALLET_ADDRESS}`)

    log.info('Waiting for networkidle...')
    await page.waitForLoadState('networkidle')

    log.info('Waiting for data...')
    const updateButton = page.locator('[class^="UpdateButton_refresh__"]')
    await updateButton.waitFor()
    await updateButton.getByText('Data updated').waitFor()

    log.info('Scraping data...')
    const assetsOnChain = await page.locator('[class^="AssetsOnChain_usdValue__"]').all()
    await Promise.all(assetsOnChain.map(async (a) =>
      a.waitFor()))
    const assetsOnChainValue = (await Promise.all(assetsOnChain.map(async (a) =>
      a.textContent())))
      .map((a) =>
        a?.replace('$', '').replace(',', ''))
      .map((a) =>
        a ? Number(a) : 0)
      .reduce(
        (a, b) =>
          a + b,
        0,
      )
    log.info('assetsOnChainValue', assetsOnChainValue)

    log.info('Inserting net worth...')
    await addNetWorth(db, {
      date: new Date(),
      value: assetsOnChainValue,
      walletAddress: WALLET_ADDRESS,
    })

    log.info('Done!')
  } catch (error: unknown) {
    log.error(error)
  }
}

log.info('Scheduling importer...')
Fx.runFork(Fx.repeat(Fx.tryPromise(task), Schedule.secondOfMinute(0)))
