import { Context, Data, type Effect, Layer } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import type {
  Candlestick,
  InvalidPriceRelationshipError,
  InvalidTimestampError,
  InvalidVolumeError,
  UnknownError,
} from '../domain/market-data/Candlestick'
import type { ExchangeId } from '../domain/market-data/ExchangeId'
import type { Pair } from '../domain/market-data/Pair'
import type { Timeframe } from '../domain/market-data/Timeframe'
import { CryptoDataServiceLive } from './CryptoDataServiceLive'

export class UnsupportedExchangeError extends Data.TaggedError(
  'UnsupportedExchangeError',
)<{
  readonly exchangeId: ExchangeId
}> {}

export interface CryptoDataServiceType {
  /**
   * Fetches candlestick data from a cryptocurrency exchange
   * @returns An array of validated Candlestick domain models
   */
  getOHLCV(
    exchangeId: ExchangeId,
    exchangeSymbol: Pair,
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
    | UnknownError
  >
}

export class CryptoDataService extends Context.Tag('CryptoDataService')<
  CryptoDataServiceType,
  CryptoDataServiceType
>() {
  static Live = Layer.succeed(this, CryptoDataServiceLive)
}
