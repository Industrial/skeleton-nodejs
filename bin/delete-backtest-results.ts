#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from 'zx'

await $`rm -rf user_data/backtest_results/*`
await $`rm -rf user_data/hyperopt_results/*`
await $`rm -rf user_data/logs/*`
await $`rm -rf user_data/plot/*`
