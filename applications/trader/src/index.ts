import { Command } from '@effect/cli'
import { BunContext, BunRuntime } from '@effect/platform-bun'
import { Console, Effect } from 'effect'

const command = Command.make('hello-world', {}, () =>
  Console.log('Hello World'),
)

const cli = Command.run(command, {
  name: 'Hello World CLI',
  version: 'v1.0.0',
})

cli(process.argv).pipe(Effect.provide(BunContext.layer), BunRuntime.runMain)
