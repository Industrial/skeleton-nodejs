import { BunRuntime } from '@effect/platform-bun'
import { Effect } from 'effect'

const failure = Effect.fail('Uh oh!')

BunRuntime.runMain(failure)
