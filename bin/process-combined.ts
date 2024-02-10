#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from '../packages/child_process/src/lib/child_process.ts'
import { STRATEGY, TIMEFRAME, TIMERANGE } from './_config.ts'

await $(`bin/delete-backtest-results.ts`)
await $(`bin/autofreq.ts backtest --strategy "${STRATEGY}" --timeframe "${TIMEFRAME}" --timerange "${TIMERANGE}"`)
await $(`bin/autofreq.ts plot-profit --strategy "${STRATEGY}" --timeframe "${TIMEFRAME}" --timerange "${TIMERANGE}"`)
