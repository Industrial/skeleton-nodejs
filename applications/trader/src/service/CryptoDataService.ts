import { Context, Data, type Effect, Layer } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import type {
  Candlestick,
  InvalidPriceRelationshipError,
  InvalidTimestampError,
  InvalidVolumeError,
  UnknownError,
} from '../domain/Candlestick'
import type { ExchangeId } from '../domain/ExchangeId'
import type { ExchangeSymbol } from '../domain/ExchangeSymbol'
import type { Timeframe } from '../domain/Timeframe'
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
    exchangeSymbol: ExchangeSymbol,
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

// biome-ignore lint/complexity/noStaticOnlyClass: <explanation>
export class CryptoDataService extends Context.Tag('CryptoDataService')<
  CryptoDataServiceType,
  CryptoDataServiceType
>() {
  static Live = Layer.succeed(this, CryptoDataServiceLive)
}
