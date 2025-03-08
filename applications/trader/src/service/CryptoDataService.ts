import type { OHLCV } from 'ccxt'
import { Context, type Effect, Layer } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import type { ExchangeId } from '../domain/ExchangeId'
import type { ExchangeSymbol } from '../domain/ExchangeSymbol'
import type { Timeframe } from '../domain/Timeframe'
import type { UnsupportedExchangeError } from '../error/UnsupportedExchangeError'
import { CryptoDataServiceLive } from './CryptoDataServiceLive'

export interface CryptoDataServiceType {
  getOHLCV(
    exchangeId: ExchangeId,
    exchangeSymbol: ExchangeSymbol,
    timeframe: Timeframe,
    start: Date,
    end: Date,
  ): Effect.Effect<OHLCV[], UnsupportedExchangeError | ParseError>
}

export class CryptoDataService extends Context.Tag('CryptoDataService')<
  CryptoDataServiceType,
  {
    readonly getOHLCV: (
      exchangeId: ExchangeId,
      exchangeSymbol: ExchangeSymbol,
      timeframe: Timeframe,
      start: Date,
      end: Date,
    ) => Effect.Effect<OHLCV[], UnsupportedExchangeError | ParseError>
  }
>() {
  static Live = Layer.succeed(this, CryptoDataServiceLive)
}
