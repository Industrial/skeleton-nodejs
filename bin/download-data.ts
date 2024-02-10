#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from 'zx'

import { COINS, EXCHANGE, TIMEFRAME, TIMERANGE } from './_config.ts'

await $`python -m freqtrade download-data --exchange ${EXCHANGE} --timerange ${TIMERANGE} --timeframe ${TIMEFRAME} --pairs '${COINS.join(' ')}'`
