import { Data, Effect, Schema } from 'effect'
import type { ParseError } from 'effect/ParseResult'

export class InvalidTimeframeMillisecondsError extends Data.TaggedError(
  'InvalidTimeframeMillisecondsError',
)<{
  readonly value: number
}> {}

export const TimeframeSchema = Schema.Union(
  Schema.Literal('1m'),
  Schema.Literal('3m'),
  Schema.Literal('5m'),
  Schema.Literal('15m'),
  Schema.Literal('30m'),
  Schema.Literal('1h'),
  Schema.Literal('2h'),
  Schema.Literal('4h'),
  Schema.Literal('8h'),
  Schema.Literal('1d'),
)

export const TimeframeSchemaValues = TimeframeSchema.members.flatMap(
  (x) => x.literals,
)

export type Timeframe = Schema.Schema.Type<typeof TimeframeSchema>

/**
 * Error thrown when an invalid timeframe is provided
 */
export class InvalidTimeframeError extends Data.TaggedError(
  'InvalidTimeframeError',
)<{
  readonly value: string
}> {}

/**
 * Validates a string as a timeframe
 * @param timeframe The timeframe string to validate
 * @returns Effect containing the validated timeframe
 */
export const validateTimeframe = (
  timeframe: string,
): Effect.Effect<Timeframe, InvalidTimeframeError, never> =>
  Effect.catchAll(Schema.decode(TimeframeSchema)(timeframe as Timeframe), () =>
    Effect.fail(new InvalidTimeframeError({ value: timeframe })),
  )

const TIMEFRAME_MS: { readonly [K in Timeframe]: number } = {
  '1m': 60 * 1000,
  '3m': 3 * 60 * 1000,
  '5m': 5 * 60 * 1000,
  '15m': 15 * 60 * 1000,
  '30m': 30 * 60 * 1000,
  '1h': 60 * 60 * 1000,
  '2h': 2 * 60 * 60 * 1000,
  '4h': 4 * 60 * 60 * 1000,
  '8h': 8 * 60 * 60 * 1000,
  '1d': 24 * 60 * 60 * 1000,
} as const

export const toMilliseconds = (
  timeframe: Timeframe,
): Effect.Effect<number, ParseError, never> =>
  Effect.gen(function* ($) {
    const validated = yield* $(Schema.decode(TimeframeSchema)(timeframe))
    return TIMEFRAME_MS[validated]
  })

export const fromMilliseconds = (
  ms: number,
): Effect.Effect<
  Timeframe,
  InvalidTimeframeMillisecondsError | ParseError,
  never
> =>
  Effect.gen(function* ($) {
    const entries = Object.entries(TIMEFRAME_MS) as Array<[Timeframe, number]>
    const exactMatch = entries.find(([_, tfMs]) => tfMs === ms)

    if (!exactMatch) {
      return yield* $(
        Effect.fail(new InvalidTimeframeMillisecondsError({ value: ms })),
      )
    }

    return yield* $(Schema.decode(TimeframeSchema)(exactMatch[0]))
  })
