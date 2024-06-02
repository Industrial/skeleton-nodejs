import { safeDivide } from '@code9/number'
import { Option as O, pipe } from 'effect'

export type TimeUnit = 'd' | 'h' | 'm' | 'ms' | 's'

/**
 * A mapping of time units to their equivalent value in milliseconds.
 */
const conversions: Record<TimeUnit, number> = {
  ms: 1,
  s: 1000,
  m: 60000,
  h: 3600000,
  d: 86400000,
}

/**
 * Converts a time value from one unit to another.
 *
 * @param value - The numeric value of the time to convert.
 * @param sourceUnit - The unit of the input time value ('d' for days, 'h' for
 *                     hours, 'm' for minutes, 'ms' for milliseconds, 's' for seconds).
 * @param targetUnit - The unit to convert the time value to ('d' for days, 'h'
 *                     for hours, 'm' for minutes, 'ms' for milliseconds, 's' for seconds).
 * @returns An Option containing the converted time value, or None if the conversion is not possible.
 *
 * @example
 * ```typescript
 * import { convertTime } from './date'
 * import { unsafeUnwrap } from 'effect'
 *
 * const result = convertTime(2, 'h', 'm')
 * console.log(unsafeUnwrap(result)) // 120
 * ```
 */
export const convertTime = <A extends number>(
  sourceUnit: TimeUnit,
  targetUnit: TimeUnit,
) =>
    (a: A): O.Option<A> =>
      sourceUnit === targetUnit
        ? O.some(a)
        : pipe(
          O.fromNullable(conversions[sourceUnit]),
          O.flatMap((source) =>
            pipe(
              O.fromNullable(conversions[targetUnit]),
              O.flatMap((target) =>
                safeDivide(a * source, target)),
            )),
        ) as O.Option<A>
