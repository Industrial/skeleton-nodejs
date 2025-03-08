/**
 * @module Candlestick
 * @description Domain model for candlestick data in trading applications.
 * Provides validation and type-safe handling of OHLCV (Open, High, Low, Close, Volume) data.
 */

import { Data, Effect, Schema } from 'effect'
import type { ParseError } from 'effect/ParseResult'

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
 * Base structure for candlestick data with price and volume information
 */
export interface CandlestickStructure {
  /** Unix timestamp in milliseconds */
  timestamp: number
  /** Opening price of the period */
  open: number
  /** Highest price of the period */
  high: number
  /** Lowest price of the period */
  low: number
  /** Closing price of the period */
  close: number
  /** Trading volume of the period */
  volume: number
}

/**
 * CCXT library OHLCV data structure
 * [timestamp, open, high, low, close, volume]
 */
export type CCXTOHLCVStructure = readonly [
  number,
  number,
  number,
  number,
  number,
  number,
]

/**
 * Schema for validating candlestick data
 * Ensures:
 * - Timestamp is positive
 * - Volume is non-negative
 * - Price relationships are valid (high >= open/close >= low)
 */
export const CandlestickSchema = Schema.Struct({
  timestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Timestamp must be positive',
    }),
  ),
  open: Schema.Number,
  high: Schema.Number,
  low: Schema.Number,
  close: Schema.Number,
  volume: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Volume must be non-negative',
    }),
  ),
}).pipe(
  Schema.filter(
    (
      candlestick: CandlestickStructure,
    ): candlestick is CandlestickStructure => {
      const { high, low, open, close } = candlestick
      return (
        high >= open &&
        high >= close &&
        low <= open &&
        low <= close &&
        high >= low
      )
    },
    {
      message: () => 'Invalid price relationship in candlestick',
      identifier: 'InvalidPriceRelationship',
    },
  ),
)

/** Type representing a validated candlestick */
export type Candlestick = Schema.Schema.Type<typeof CandlestickSchema>

/**
 * Creates a validated candlestick from raw data
 * @param params - Raw candlestick data
 * @returns Effect containing either a validated candlestick or an error
 * @throws {InvalidPriceRelationshipError} When price relationships are invalid
 * @throws {InvalidVolumeError} When volume is negative or other validation fails
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
  params: CandlestickStructure,
): Effect.Effect<
  Candlestick,
  InvalidPriceRelationshipError | InvalidVolumeError,
  never
> =>
  Effect.try({
    try: () => Schema.decodeSync(CandlestickSchema)(params),
    catch: (error: unknown) => {
      const parseError = error as ParseError
      if (parseError.message?.includes('InvalidPriceRelationship')) {
        return new InvalidPriceRelationshipError({
          message: `Invalid price relationship in candlestick: high=${params.high}, low=${params.low}, open=${params.open}, close=${params.close}`,
        })
      }
      return new InvalidVolumeError({
        message: String(error),
      })
    },
  })

/**
 * Creates a validated candlestick from CCXT OHLCV format
 * @param ohlcv - CCXT OHLCV array [timestamp, open, high, low, close, volume]
 * @returns Effect containing either a validated candlestick or an error
 * @throws {InvalidPriceRelationshipError} When price relationships are invalid
 * @throws {InvalidVolumeError} When volume is negative or other validation fails
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
  ohlcv: CCXTOHLCVStructure,
): Effect.Effect<
  Candlestick,
  InvalidPriceRelationshipError | InvalidVolumeError,
  never
> =>
  createCandlestick({
    timestamp: ohlcv[0],
    open: ohlcv[1],
    high: ohlcv[2],
    low: ohlcv[3],
    close: ohlcv[4],
    volume: ohlcv[5],
  })
