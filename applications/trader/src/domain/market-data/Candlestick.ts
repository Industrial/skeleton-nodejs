/**
 * @module Candlestick
 * @description Domain model for candlestick data in trading applications.
 * Provides validation and type-safe handling of OHLCV (Open, High, Low, Close, Volume) data.
 */

import type { OHLCV } from 'ccxt'
import { Data, Effect, Schema } from 'effect'

/**
 * Error thrown when strategy parameters are invalid
 */
export class InvalidCandlestickError extends Data.TaggedError(
  'InvalidCandlestickError',
)<{
  readonly message: string
}> {}

/**
 * Error thrown when candlestick price relationships are invalid.
 * Valid relationships are: high >= open, high >= close, low <= open, low <= close, high >= low
 */
export class InvalidPriceRelationshipError extends Data.TaggedError(
  'InvalidPriceRelationshipError',
)<{
  readonly message: string
}> {}

/**
 * Error thrown when candlestick volume is invalid (negative)
 */
export class InvalidVolumeError extends Data.TaggedError('InvalidVolumeError')<{
  readonly message: string
}> {}

/**
 * Error thrown when candlestick timestamp is invalid (non-positive)
 * Valid timestamps must be positive numbers representing Unix time in milliseconds
 */
export class InvalidTimestampError extends Data.TaggedError(
  'InvalidTimestampError',
)<{
  readonly message: string
}> {}

/**
 * Basic schema for candlestick data structure
 */
export const CandlestickBaseSchema = Schema.Struct({
  timestamp: Schema.Number,
  open: Schema.Number,
  high: Schema.Number,
  low: Schema.Number,
  close: Schema.Number,
  volume: Schema.Number,
})

/**
 * Combined schema for validating candlestick data
 * Ensures:
 * - Timestamp is positive
 * - Volume is non-negative
 * - Price relationships are valid (high >= open/close >= low)
 */
export const CandlestickSchema = CandlestickBaseSchema

/** Type representing a validated candlestick */
export type Candlestick = Schema.Schema.Type<typeof CandlestickSchema>

/** Encoded representation */
export type CandlestickEncoded = Schema.Schema.Encoded<typeof CandlestickSchema>

/**
 * Creates a validated candlestick from raw data
 * @param params - Raw candlestick data
 * @returns Effect containing either a validated candlestick or an error
 * @throws {InvalidPriceRelationshipError} When price relationships are invalid
 * @throws {InvalidVolumeError} When volume is negative
 * @throws {InvalidTimestampError} When timestamp is non-positive
 * @example
 * ```ts
 * const result = createCandlestick({
 *   timestamp: Date.now(),
 *   open: 100,
 *   high: 110,
 *   low: 90,
 *   close: 105,
 *   volume: 1000
 * })
 * ```
 */
export const createCandlestick = (
  params: Candlestick,
): Effect.Effect<
  Candlestick,
  | InvalidCandlestickError
  | InvalidPriceRelationshipError
  | InvalidTimestampError
  | InvalidVolumeError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate timestamp
    if (params.timestamp <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTimestampError({
            message: `Invalid timestamp in candlestick: timestamp=${params.timestamp}`,
          }),
        ),
      )
    }

    // Validate volume
    if (params.volume < 0) {
      return yield* _(
        Effect.fail(
          new InvalidVolumeError({
            message: 'Volume must be non-negative',
          }),
        ),
      )
    }

    // Validate price relationships
    const { high, low, open, close } = params
    if (
      high < open ||
      high < close ||
      low > open ||
      low > close ||
      high < low
    ) {
      return yield* _(
        Effect.fail(
          new InvalidPriceRelationshipError({
            message: `Invalid price relationship in candlestick: high=${high}, low=${low}, open=${open}, close=${close}`,
          }),
        ),
      )
    }

    // Validate using the schema
    try {
      const decoded = Schema.decodeSync(CandlestickSchema)(params)

      return decoded
    } catch (error: unknown) {
      return yield* _(
        Effect.fail(
          new InvalidCandlestickError({
            message: `Invalid strategy parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Creates a validated candlestick from CCXT OHLCV format
 * @param ohlcv - CCXT OHLCV array [timestamp, open, high, low, close, volume]
 * @returns Effect containing either a validated candlestick or an error
 * @throws {InvalidPriceRelationshipError} When price relationships are invalid
 * @throws {InvalidVolumeError} When volume is negative
 * @throws {InvalidTimestampError} When timestamp is non-positive
 * @example
 * ```ts
 * const result = fromCCXT([
 *   1625097600000, // timestamp
 *   100,           // open
 *   110,           // high
 *   90,            // low
 *   105,           // close
 *   1000           // volume
 * ])
 * ```
 */
export const fromCCXT = (
  ohlcv: OHLCV,
): Effect.Effect<
  Candlestick,
  | InvalidCandlestickError
  | InvalidPriceRelationshipError
  | InvalidTimestampError
  | InvalidVolumeError,
  never
> =>
  createCandlestick({
    timestamp: Number(ohlcv[0]),
    open: Number(ohlcv[1]),
    high: Number(ohlcv[2]),
    low: Number(ohlcv[3]),
    close: Number(ohlcv[4]),
    volume: Number(ohlcv[5]),
  })
