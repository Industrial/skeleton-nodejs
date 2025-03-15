import * as ccxt from 'ccxt'
import { Effect } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import { isUndefined } from 'effect/Predicate'
import {
  type Candlestick,
  type InvalidCandlestickError,
  type InvalidPriceRelationshipError,
  type InvalidTimestampError,
  type InvalidVolumeError,
  fromCCXT,
} from '../domain/market-data/Candlestick'
import type { ExchangeId } from '../domain/market-data/ExchangeId'
import type { Pair } from '../domain/market-data/Pair'
import type { Timeframe } from '../domain/market-data/Timeframe'
import { toMilliseconds } from '../domain/market-data/Timeframe'
import { DateRangeError, UnsupportedExchangeError } from './CryptoDataService'

/**
 * Gets the exchange class for a given exchange ID
 * @param exchangeId The ID of the exchange
 * @returns The exchange class constructor or undefined if not found
 */
export const getExchangeClass = (
  exchangeId: ExchangeId,
): Effect.Effect<new () => ccxt.Exchange, UnsupportedExchangeError, never> =>
  Effect.gen(function* ($) {
    const ExchangeClass =
      ccxt.exchanges[exchangeId as keyof typeof ccxt.exchanges]

    if (isUndefined(ExchangeClass)) {
      return yield* $(
        Effect.fail(
          new UnsupportedExchangeError({
            exchangeId,
          }),
        ),
      )
    }

    return ExchangeClass as new () => ccxt.Exchange
  })

export const getOHLCV = (
  exchangeId: ExchangeId,
  pair: Pair,
  timeframe: Timeframe,
  start: Date,
  end: Date,
): Effect.Effect<
  Candlestick[],
  | DateRangeError
  | InvalidCandlestickError
  | InvalidPriceRelationshipError
  | InvalidTimestampError
  | InvalidVolumeError
  | ParseError
  | UnsupportedExchangeError,
  never
> =>
  Effect.gen(function* ($) {
    // Check the start and end date
    if (start.valueOf() >= end.valueOf()) {
      return yield* $(
        new DateRangeError({
          start,
          end,
        }),
      )
    }

    // Get exchange instance
    const ExchangeClass = yield* $(getExchangeClass(exchangeId))
    const exchange = new ExchangeClass() as ccxt.Exchange

    // Calculate parameters for CCXT API call
    const since = start.getTime()
    const timeDiff = end.getTime() - since
    const timeframeMs: number = yield* $(toMilliseconds(timeframe))
    const limit = Math.ceil(timeDiff / timeframeMs)

    // Fetch raw OHLCV data from CCXT
    const rawData = yield* $(
      Effect.promise(() => exchange.fetchOHLCV(pair, timeframe, since, limit)),
    )

    // Convert each CCXT OHLCV array to our Candlestick domain model
    const candlesticks = yield* $(
      Effect.forEach(rawData, (ohlcv) => fromCCXT(ohlcv)),
    )

    return candlesticks
  })
