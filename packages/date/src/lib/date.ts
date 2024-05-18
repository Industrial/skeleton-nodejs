import { safeDivide } from '@code9/number'
import { Option as O } from 'effect'
import { pipe } from 'fp-ts/function'

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
): O.Option<number> =>
  sourceUnit === targetUnit
    ? O.some(value)
    : pipe(
      O.fromNullable(conversions[sourceUnit]),
      O.flatMap((source) =>
        pipe(
          O.fromNullable(conversions[targetUnit]),
          O.flatMap((target) =>
            safeDivide(value * source, target)),
        )),
    )
