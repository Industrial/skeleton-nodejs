import * as ccxt from 'ccxt'
import { Effect } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import { isUndefined } from 'effect/Predicate'
import type { ExchangeId } from '../domain/ExchangeId'
import type { ExchangeSymbol } from '../domain/ExchangeSymbol'
import type { Timeframe } from '../domain/Timeframe'
import { toMilliseconds } from '../domain/Timeframe'
import { UnsupportedExchangeError } from '../error/UnsupportedExchangeError'

const getOHLCV = (
  exchangeId: ExchangeId,
  symbol: ExchangeSymbol,
  timeframe: Timeframe,
  start: Date,
  end: Date,
): Effect.Effect<ccxt.OHLCV[], UnsupportedExchangeError | ParseError, never> =>
  Effect.gen(function* ($) {
    const ExchangeClass =
      ccxt.exchanges[exchangeId as keyof typeof ccxt.exchanges]

    if (isUndefined(ExchangeClass)) {
      return yield* $(Effect.fail(new UnsupportedExchangeError()))
    }

    const exchange = new ExchangeClass() as ccxt.Exchange

    const since = start.getTime()
    const timeDiff = end.getTime() - since
    const timeframeMs: number = yield* $(toMilliseconds(timeframe))
    const limit = Math.ceil(timeDiff / timeframeMs)

    const result = Effect.promise(() =>
      exchange.fetchOHLCV(symbol, timeframe, since, limit),
    )

    return yield* $(result)
  })

export const CryptoDataServiceLive = {
  getOHLCV,
}
