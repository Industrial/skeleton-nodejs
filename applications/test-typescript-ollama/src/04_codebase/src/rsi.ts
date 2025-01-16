import { Effect as E, pipe } from 'effect'

import { rma } from './rma.ts'

// Helper function to calculate returns.
const returns = (values: Array<number>): Array<number> =>
  values
    .map((value, index) => {
      const lastValue = values[index - 1] ?? 0
      return index === 0 ? 0 : value - lastValue
    })
    .slice(1)

// Reimplemented RSI function using Effect.
export const rsi =
  (length: number) =>
  (values: E.Effect<Array<number>>): E.Effect<Array<number>> =>
    pipe(
      values,
      E.flatMap((as) => {
        if (as.length === 0) {
          return E.succeed([])
        }

        const changes = returns(as)

        const ups = rma(length)(E.succeed(changes.map((c) => Math.max(c, 0))))
        const downs = rma(length)(
          E.succeed(changes.map((c) => -Math.min(c, 0))),
        )

        return pipe(
          E.zipWith(ups, downs, (upArray, downArray) =>
            downArray.map((down, index) => {
              const up = upArray[index] ?? 0
              return down === 0
                ? 100
                : up === 0
                  ? 0
                  : 100 - 100 / (1 + up / down)
            }),
          ),
        )
      }),
    )
