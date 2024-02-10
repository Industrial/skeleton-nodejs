#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { function as FN, taskEither as TE } from 'fp-ts'

import { retry } from '../lib/child_process.ts'

const useDocker = process.env['AUTOFREQ_USE_DOCKER'] === 'true'

const command = useDocker ? `bin/autofreq-docker.ts ${process.argv.slice(2).join(' ')}` : `python cli.py ${process.argv.slice(2).join(' ')}`

await FN.pipe(retry(command), TE.match(console.error, FN.identity))()
