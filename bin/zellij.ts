#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { function as F, taskEither as TE } from 'fp-ts'

import { spawn } from '../packages/child_process/src/lib/child_process.ts'

const continueRegardless = (a: TE.TaskEither<Error, void>) => F.pipe(a,
  TE.fold(() => TE.right(undefined),
    () => TE.right(undefined)))

await F.pipe(spawn(`zellij kill-session autofreq`),
  continueRegardless,
  TE.chain(() => spawn(`zellij --layout zellij.kdl --session autofreq`)),
  continueRegardless)()
