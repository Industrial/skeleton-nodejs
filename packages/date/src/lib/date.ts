import { safeDivide } from '@code9/number'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'

const conversions: Record<string, number> = {
  ms: 1,
  s: 1000,
  m: 60000,
  h: 3600000,
  d: 86400000,
}

export const convertTime = (
  value: number,
  sourceUnit: 'd' | 'h' | 'm' | 'ms' | 's',
  targetUnit: 'd' | 'h' | 'm' | 'ms' | 's',
): O.Option<number> => {
  if (sourceUnit === targetUnit) {
    return O.some(value)
  }

  return pipe(
    O.fromNullable(conversions[sourceUnit]),
    O.chain((source) => {
      return pipe(
        O.fromNullable(conversions[targetUnit]),
        O.chain((target) => {
          return safeDivide(value * source, target)
        }),
      )
    }),
  )
}
