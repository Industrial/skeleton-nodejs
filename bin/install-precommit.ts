#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from '../packages/child_process/src/lib/child_process.ts'

await $(`pre-commit install`)
