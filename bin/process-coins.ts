#!/usr/bin/env -S node --import '@swc-node/register/esm-register'
import 'dotenv/config'

import { promises as fs } from 'fs'

import { fileExists } from '../lib/fs.ts'
import { $ } from '../packages/child_process/src/lib/child_process.ts'
import { COINS, STRATEGY, STRATEGY_DIR, TIMEFRAME, TIMERANGE } from './_config.ts'

await $(`bin/delete-backtest-results.ts`)

await fs.rm(`${STRATEGY_DIR}/log/${TIMEFRAME}`, { recursive: true })
await fs.mkdir(`${STRATEGY_DIR}/log/${TIMEFRAME}`, { recursive: true })

await fs.rm(`${STRATEGY_DIR}/opt/${TIMEFRAME}`, { recursive: true })
await fs.mkdir(`${STRATEGY_DIR}/opt/${TIMEFRAME}`, { recursive: true })

await fs.rm(`${STRATEGY_DIR}/plot/${TIMEFRAME}`, { recursive: true })
await fs.mkdir(`${STRATEGY_DIR}/plot/${TIMEFRAME}`, { recursive: true })

let i = 0
const l = COINS.length
for (const coin of COINS) {
  try {
    i += 1
    console.log(`[${i}/${l}] ${coin}`)

    const coinFilename = coin.replace('/', '_')
    const hyperoptFilePath = `${STRATEGY_DIR}/opt/${TIMEFRAME}/${coinFilename}.json`

    if (!await fileExists(hyperoptFilePath)) {
      await $(`bin/autofreq.ts hyperopt --strategy=${STRATEGY} --timeframe=${TIMEFRAME} --timerange=${TIMERANGE} --pairs=${coin} --epochs=100`)
      await $(`mv ${STRATEGY_DIR}/${STRATEGY}.json ${hyperoptFilePath}`)
      await $(`mv ${STRATEGY_DIR}/log/${TIMEFRAME}/hyperopt.log ${STRATEGY_DIR}/log/${TIMEFRAME}/${coinFilename}-hyperopt.log`)
    }

    const backtestLogFilePath = `${STRATEGY_DIR}/log/${TIMEFRAME}/${coinFilename}-backtest.log`

    if (!(await fileExists(backtestLogFilePath))) {
      await $(`bin/autofreq.ts backtest --strategy=${STRATEGY} --timeframe=${TIMEFRAME} --timerange=${TIMERANGE} --pairs=${coin}`)
      await $(`mv ${STRATEGY_DIR}/log/${TIMEFRAME}/backtest.log ${backtestLogFilePath}`)
    }

    const plotDataframeLogFilePath = `${STRATEGY_DIR}/log/${TIMEFRAME}/${coinFilename}-plot-dataframe.log`
    const plotProfitLogFilePath = `${STRATEGY_DIR}/log/${TIMEFRAME}/${coinFilename}-plot-profit.log`

    if (!(await fileExists(plotDataframeLogFilePath))) {
      await $(`bin/autofreq.ts plot --strategy=${STRATEGY} --timeframe=${TIMEFRAME} --timerange=${TIMERANGE} --pairs=${coin}`)
      await $(`mv ${STRATEGY_DIR}/log/${TIMEFRAME}/plot-dataframe.log ${plotDataframeLogFilePath}`)
      await $(`mv ${STRATEGY_DIR}/log/${TIMEFRAME}/plot-profit.log ${plotProfitLogFilePath}`)
    }
  } catch (error: unknown) {
    console.error(error)
  }
}
