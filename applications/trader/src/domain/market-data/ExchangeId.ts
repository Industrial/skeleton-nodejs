import { Data, Effect, Either, Schema } from 'effect'

/**
 * Error thrown when an invalid exchange ID is provided
 */
export class InvalidExchangeIdError extends Data.TaggedError(
  'InvalidExchangeIdError',
)<{
  readonly value: string
}> {}

// A literal list is pretty horrible, but that's what it takes at this point. I
// should maybe refactor this but I don't know how to get the types to be
// correct.
export const ExchangeIdSchema = Schema.Union(
  Schema.Literal('ace'),
  Schema.Literal('alpaca'),
  Schema.Literal('ascendex'),
  Schema.Literal('bequant'),
  Schema.Literal('bigone'),
  Schema.Literal('binance'),
  Schema.Literal('binancecoinm'),
  Schema.Literal('binanceus'),
  Schema.Literal('binanceusdm'),
  Schema.Literal('bingx'),
  Schema.Literal('bit2c'),
  Schema.Literal('bitbank'),
  Schema.Literal('bitbns'),
  Schema.Literal('bitcoincom'),
  Schema.Literal('bitfinex'),
  Schema.Literal('bitfinex1'),
  Schema.Literal('bitflyer'),
  Schema.Literal('bitget'),
  Schema.Literal('bithumb'),
  Schema.Literal('bitmart'),
  Schema.Literal('bitmex'),
  Schema.Literal('bitopro'),
  Schema.Literal('bitpanda'),
  Schema.Literal('bitrue'),
  Schema.Literal('bitso'),
  Schema.Literal('bitstamp'),
  Schema.Literal('bitteam'),
  Schema.Literal('bitvavo'),
  Schema.Literal('bl3p'),
  Schema.Literal('blockchaincom'),
  Schema.Literal('blofin'),
  Schema.Literal('btcalpha'),
  Schema.Literal('btcbox'),
  Schema.Literal('btcmarkets'),
  Schema.Literal('btcturk'),
  Schema.Literal('bybit'),
  Schema.Literal('cex'),
  Schema.Literal('coinbase'),
  Schema.Literal('coinbaseadvanced'),
  Schema.Literal('coinbaseexchange'),
  Schema.Literal('coinbaseinternational'),
  Schema.Literal('coincatch'),
  Schema.Literal('coincheck'),
  Schema.Literal('coinex'),
  Schema.Literal('coinlist'),
  Schema.Literal('coinmate'),
  Schema.Literal('coinmetro'),
  Schema.Literal('coinone'),
  Schema.Literal('coinsph'),
  Schema.Literal('coinspot'),
  Schema.Literal('cryptocom'),
  Schema.Literal('defx'),
  Schema.Literal('delta'),
  Schema.Literal('deribit'),
  Schema.Literal('digifinex'),
  Schema.Literal('ellipx'),
  Schema.Literal('exmo'),
  Schema.Literal('fmfwio'),
  Schema.Literal('gate'),
  Schema.Literal('gateio'),
  Schema.Literal('gemini'),
  Schema.Literal('hashkey'),
  Schema.Literal('hitbtc'),
  Schema.Literal('hollaex'),
  Schema.Literal('htx'),
  Schema.Literal('huobi'),
  Schema.Literal('huobijp'),
  Schema.Literal('hyperliquid'),
  Schema.Literal('idex'),
  Schema.Literal('independentreserve'),
  Schema.Literal('indodax'),
  Schema.Literal('kraken'),
  Schema.Literal('krakenfutures'),
  Schema.Literal('kucoin'),
  Schema.Literal('kucoinfutures'),
  Schema.Literal('kuna'),
  Schema.Literal('latoken'),
  Schema.Literal('lbank'),
  Schema.Literal('luno'),
  Schema.Literal('mercado'),
  Schema.Literal('mexc'),
  Schema.Literal('myokx'),
  Schema.Literal('ndax'),
  Schema.Literal('novadax'),
  Schema.Literal('oceanex'),
  Schema.Literal('okcoin'),
  Schema.Literal('okx'),
  Schema.Literal('onetrading'),
  Schema.Literal('oxfun'),
  Schema.Literal('p2b'),
  Schema.Literal('paradex'),
  Schema.Literal('paymium'),
  Schema.Literal('phemex'),
  Schema.Literal('poloniex'),
  Schema.Literal('poloniexfutures'),
  Schema.Literal('probit'),
  Schema.Literal('timex'),
  Schema.Literal('tokocrypto'),
  Schema.Literal('tradeogre'),
  Schema.Literal('upbit'),
  Schema.Literal('vertex'),
  Schema.Literal('wavesexchange'),
  Schema.Literal('whitebit'),
  Schema.Literal('woo'),
  Schema.Literal('woofipro'),
  Schema.Literal('xt'),
  Schema.Literal('yobit'),
  Schema.Literal('zaif'),
  Schema.Literal('zonda'),
)

/**
 * List of all valid exchange ID literals
 */
export const ExchangeIdSchemaValues = ExchangeIdSchema.members.flatMap(
  (x) => x.literals,
)

/**
 * Type for validated exchange ID
 */
export type ExchangeId = Schema.Schema.Type<typeof ExchangeIdSchema>

/**
 * Validates a string as an exchange ID
 * @param exchangeId The exchange ID string to validate
 * @returns Effect containing the validated exchange ID
 */
export const validateExchangeId = (
  exchangeId: string,
): Effect.Effect<ExchangeId, InvalidExchangeIdError, never> =>
  Effect.catchAll(
    Schema.decode(ExchangeIdSchema)(exchangeId as ExchangeId),
    () => Effect.fail(new InvalidExchangeIdError({ value: exchangeId })),
  )

/**
 * Creates an exchange ID with error handling
 * @param exchangeId The exchange ID string to create
 * @returns Effect containing the validated exchange ID
 */
export const createExchangeId = (
  exchangeId: string,
): Effect.Effect<ExchangeId, InvalidExchangeIdError, never> =>
  Effect.gen(function* ($) {
    if (!exchangeId) {
      return yield* $(
        Effect.fail(new InvalidExchangeIdError({ value: exchangeId })),
      )
    }

    try {
      return yield* $(validateExchangeId(exchangeId))
    } catch (error) {
      return yield* $(
        Effect.fail(new InvalidExchangeIdError({ value: exchangeId })),
      )
    }
  })

/**
 * Checks if a string is a valid exchange ID
 * @param exchangeId The exchange ID string to check
 * @returns Effect containing a boolean indicating if the exchange ID is valid
 */
export const isValidExchangeId = (
  exchangeId: string,
): Effect.Effect<boolean, never, never> =>
  Effect.gen(function* ($) {
    const result = yield* $(Effect.either(validateExchangeId(exchangeId)))
    return Either.isRight(result)
  })

/**
 * Returns a human-readable display name for an exchange ID
 * @param exchangeId The exchange ID
 * @returns Effect containing the display name
 */
export const getExchangeDisplayName = (
  exchangeId: ExchangeId,
): Effect.Effect<string, never, never> =>
  Effect.sync(() => {
    // Map of exchange IDs to display names
    const displayNames: Record<string, string> = {
      binance: 'Binance',
      binanceus: 'Binance US',
      binanceusdm: 'Binance USDⓈ-M',
      binancecoinm: 'Binance COIN-M',
      coinbase: 'Coinbase',
      coinbaseadvanced: 'Coinbase Advanced',
      coinbaseexchange: 'Coinbase Exchange',
      coinbaseinternational: 'Coinbase International',
      kraken: 'Kraken',
      krakenfutures: 'Kraken Futures',
      kucoin: 'KuCoin',
      kucoinfutures: 'KuCoin Futures',
      bybit: 'Bybit',
      okx: 'OKX',
      // Add more mappings as needed
    }

    // Return the display name if it exists, otherwise capitalize the first letter
    return (
      displayNames[exchangeId] ||
      exchangeId.charAt(0).toUpperCase() + exchangeId.slice(1)
    )
  })
