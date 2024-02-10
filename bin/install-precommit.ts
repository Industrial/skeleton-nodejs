#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from '../lib/child_process.ts'

await $(`pre-commit install`)
