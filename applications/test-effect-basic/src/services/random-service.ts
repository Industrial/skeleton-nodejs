import { Context, Effect } from 'effect'

export class RandomService extends Context.Tag('MyRandomService')<
  RandomService,
  { readonly next: Effect.Effect<number> }
>() {}

export const program = Effect.gen(function* () {
  const random = yield* RandomService
  const randomNumber = yield* random.next
  console.log(`random number: ${randomNumber}`)
})

export const runnable = Effect.provideService(program, RandomService, {
  next: Effect.sync(() => Math.random()),
})

await Effect.runPromise(runnable)
