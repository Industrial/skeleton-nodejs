import { safeDivide } from '@code9/number'
import { Option } from 'effect'
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
): Option.Option<number> =>
  sourceUnit === targetUnit
    ? Option.some(value)
    : pipe(
      Option.fromNullable(conversions[sourceUnit]),
      Option.flatMap((source) =>
        pipe(
          Option.fromNullable(conversions[targetUnit]),
          Option.flatMap((target) =>
            safeDivide(value * source, target)),
        )),
    )
