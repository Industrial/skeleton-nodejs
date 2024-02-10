#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { $ } from 'zx'

await $`docker-compose run --rm freqtrade bin/autofreq.ts "$@"`
