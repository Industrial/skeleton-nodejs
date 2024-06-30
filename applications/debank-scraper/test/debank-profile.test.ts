import { log } from '@code9/log'
import { isNull } from '@code9/null'
import { assertIsNotUndefined } from '@code9/undefined'
import { expect, test } from '@playwright/test'

import { db } from '~/lib/db'
import { findWalletByAddress } from '~/lib/db/generated/queries'

const { WALLET_ADDRESS } = process.env
assertIsNotUndefined(WALLET_ADDRESS)

test('has title', async ({ page }) => {
  await page.goto('https://playwright.dev/')

  log.info('Fetching wallet...')
  const wallet = await findWalletByAddress(db, {
    address: WALLET_ADDRESS,
  })
  if (isNull(wallet)) {
    throw new Error('Wallet not found')
  }
  log.info('wallet', wallet.id)

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
  await Promise.all(assetsOnChain.map((a) =>
    a.waitFor()))
  const assetsOnChainValue = (await Promise.all(assetsOnChain.map((a) =>
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

  // log.info('Waiting for selector...')
  // await page.locator('[class^="HeaderInfo_totalAssetInner__"]').click()

  // log.info('Clicked!')

  // const chartElement = await page.$('.recharts-wrapper')
  // if (isNull(chartElement)) {
  //   throw new Error('Chart element not found')
  // }

  // await chartElement.click()
  // log.info('chartElement', chartElement)

  // Expect a title "to contain" a substring.
  await expect(page).toHaveTitle(/Playwright/u)
})
