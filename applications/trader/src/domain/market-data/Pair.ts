import { Data, Effect, Schema } from 'effect'
import type { ParseError } from 'effect/ParseResult'

/**
 * Error thrown when an invalid trading pair is provided
 */
export class InvalidPairError extends Data.TaggedError('InvalidPairError')<{
  readonly value: string
}> {}

/**
 * Schema for cryptocurrency trading pairs
 * Format: BASE/QUOTE (e.g., BTC/USDT)
 */
export const PairSchema = Schema.String.pipe(
  Schema.pattern(/^[A-Za-z0-9]+\/[A-Za-z0-9]+$/, {
    message: () =>
      'Trading pair must be in the format BASE/QUOTE (e.g., BTC/USDT)',
  }),
)

/**
 * Type for validated trading pair
 */
export type Pair = Schema.Schema.Type<typeof PairSchema>

/**
 * Validates a string as a trading pair
 * @param pair The pair string to validate
 * @returns Effect containing the validated pair
 */
export const validatePair = (
  pair: string,
): Effect.Effect<Pair, ParseError, never> =>
  Effect.gen(function* ($) {
    return yield* $(Schema.decode(PairSchema)(pair))
  })

/**
 * Checks if a string follows the BASE/QUOTE format
 * @param pair The pair string to check
 * @returns Effect containing a boolean indicating if the format is valid
 */
export const isValidPairFormat = (
  pair: string,
): Effect.Effect<boolean, never, never> =>
  Effect.sync(() => {
    const parts = pair.split('/')
    return parts.length === 2 && parts[0].length > 0 && parts[1].length > 0
  })

/**
 * Extracts the base currency from a trading pair
 * @param pair The trading pair
 * @returns Effect containing the base currency
 */
export const getBaseCurrency = (
  pair: Pair,
): Effect.Effect<string, InvalidPairError, never> =>
  Effect.gen(function* ($) {
    const parts = pair.split('/')
    if (parts.length !== 2 || !parts[0]) {
      return yield* $(Effect.fail(new InvalidPairError({ value: pair })))
    }
    return parts[0]
  })

/**
 * Extracts the quote currency from a trading pair
 * @param pair The trading pair
 * @returns Effect containing the quote currency
 */
export const getQuoteCurrency = (
  pair: Pair,
): Effect.Effect<string, InvalidPairError, never> =>
  Effect.gen(function* ($) {
    const parts = pair.split('/')
    if (parts.length !== 2 || !parts[1]) {
      return yield* $(Effect.fail(new InvalidPairError({ value: pair })))
    }
    return parts[1]
  })

/**
 * Creates a pair from base and quote currencies
 * @param base The base currency
 * @param quote The quote currency
 * @returns Effect containing the pair or an error if the resulting pair is invalid
 */
export const fromCurrencies = (
  base: string,
  quote: string,
): Effect.Effect<Pair, ParseError | InvalidPairError, never> =>
  Effect.gen(function* ($) {
    if (!base || !quote) {
      return yield* $(
        Effect.fail(new InvalidPairError({ value: `${base}/${quote}` })),
      )
    }

    const pairString = `${base}/${quote}`

    // Validate the pair format
    return yield* $(validatePair(pairString))
  })
