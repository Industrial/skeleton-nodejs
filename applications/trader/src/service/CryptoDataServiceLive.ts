import * as ccxt from 'ccxt'
import { Effect } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import { isUndefined } from 'effect/Predicate'
import {
  type Candlestick,
  type InvalidPriceRelationshipError,
  type InvalidTimestampError,
  type InvalidVolumeError,
  type UnknownError,
  fromCCXT,
} from '../domain/market-data/Candlestick'
import type { ExchangeId } from '../domain/market-data/ExchangeId'
import type { Pair } from '../domain/market-data/Pair'
import type { Timeframe } from '../domain/market-data/Timeframe'
import { toMilliseconds } from '../domain/market-data/Timeframe'
import { UnsupportedExchangeError } from './CryptoDataService'

const getOHLCV = (
  exchangeId: ExchangeId,
  symbol: Pair,
  timeframe: Timeframe,
  start: Date,
  end: Date,
): Effect.Effect<
  Candlestick[],
  | UnsupportedExchangeError
  | ParseError
  | InvalidPriceRelationshipError
  | InvalidVolumeError
  | InvalidTimestampError
  | UnknownError,
  never
> =>
  Effect.gen(function* ($) {
    // 1. Get exchange instance
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

    const exchange = new ExchangeClass() as ccxt.Exchange

    // 2. Calculate parameters for CCXT API call
    const since = start.getTime()
    const timeDiff = end.getTime() - since
    const timeframeMs: number = yield* $(toMilliseconds(timeframe))
    const limit = Math.ceil(timeDiff / timeframeMs)

    // 3. Fetch raw OHLCV data from CCXT
    const rawData = yield* $(
      Effect.promise(() =>
        exchange.fetchOHLCV(symbol, timeframe, since, limit),
      ),
    )

    // 4. Convert each CCXT OHLCV array to our Candlestick domain model
    const candlesticks = yield* $(
      Effect.forEach(rawData, (ohlcv) => fromCCXT(ohlcv)),
    )

    return candlesticks
  })

export const CryptoDataServiceLive = {
  getOHLCV,
}
