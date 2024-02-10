#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from '../packages/child_process/src/lib/child_process.ts'

try {
  await $(`zellij kill-session autofreq`)
} catch (_error: unknown) {
  // console.log('No session to kill')
} finally {
  await $(`zellij --layout zellij.kdl --session autofreq`)
}
