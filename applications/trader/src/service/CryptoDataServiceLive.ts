import * as ccxt from 'ccxt'
import { Effect } from 'effect'
import { isUndefined } from 'effect/Predicate'
import type { ExchangeId } from '../domain/ExchangeId'
import type { ExchangeSymbol } from '../domain/ExchangeSymbol'
import type { Timeframe } from '../domain/Timeframe'
import { UnsupportedExchangeError } from '../error/UnsupportedExchangeError'

const getOHLCV = (
  exchangeId: ExchangeId,
  symbol: ExchangeSymbol,
  timeframe: Timeframe,
  since: Date,
  limit: number,
): Effect.Effect<ccxt.OHLCV[], UnsupportedExchangeError, never> =>
  Effect.gen(function* ($) {
    const ExchangeClass =
      ccxt.exchanges[exchangeId as keyof typeof ccxt.exchanges]

    if (isUndefined(ExchangeClass)) {
      return yield* $(Effect.fail(new UnsupportedExchangeError()))
    }

    const exchange = new ExchangeClass() as ccxt.Exchange

    return yield* $(
      Effect.promise(() =>
        exchange.fetchOHLCV(symbol, timeframe, since.valueOf(), limit),
      ),
    )
  })

export const CryptoDataServiceLive = {
  getOHLCV,
}
