#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { directoryExists } from '../lib/fs.ts'
import { $ } from '../packages/child_process/src/lib/child_process.ts'

if (!await directoryExists('.venv')) {
  await $(`python3 -m venv .venv`)
}

// await $`chmod +x .venv/bin/activate`
// await $`.venv/bin/activate`

await $(`bin/install`)

// await $`pip install -r freqtrade/requirements-hyperopt.txt`
// await $`pip install -r freqtrade/requirements-freqai-rl.txt`
// await $`pip install fire`
// await $`pip install freqtrade`
