#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { spawn } from '../packages/child_process/src/lib/child_process.ts'

try {
  await spawn(`zellij kill-session autofreq`)()
} catch (error) {
  console.error(error)
} finally {
  await spawn(`zellij --layout zellij.kdl --session autofreq`)()
}
