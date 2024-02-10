#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from '../lib/child_process.ts'

try {
  await $(`command -v pip`)
  await $(`pip install --upgrade pip`)
  await $(`pip install poetry`)
} catch (error) {
  console.log('Command pip not found')
  process.exit(1)
}

try {
  await $(`command -v poetry`)
} catch (error) {
  console.log('Command poetry not found')
  process.exit(1)
}

await $(`poetry install`)
