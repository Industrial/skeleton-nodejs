import { describe, expect, it } from 'bun:test'
import { Effect, Either, Schema } from 'effect'
import type { ExchangeId } from './ExchangeId'
import {
  ExchangeIdSchema,
  ExchangeIdSchemaValues,
  InvalidExchangeIdError,
  createExchangeId,
  getExchangeDisplayName,
  isValidExchangeId,
  validateExchangeId,
} from './ExchangeId'

describe('ExchangeId', () => {
  describe('When working with the ExchangeIdSchema', () => {
    describe('When validating exchange ID values', () => {
      it('should accept valid exchange ID values', () => {
        /* Test that valid exchange ID values are accepted */
        const validExchangeIds = [
          'binance',
          'coinbase',
          'kraken',
          'kucoin',
          'bybit',
          'okx',
        ]

        for (const exchangeId of validExchangeIds) {
          const result = Effect.runSync(
            Effect.either(
              Schema.decode(ExchangeIdSchema)(exchangeId as ExchangeId),
            ),
          )
          expect(Either.isRight(result)).toBe(true)
        }
      })

      it('should reject invalid exchange ID values', () => {
        /* Test that invalid exchange ID values are rejected */
        const invalidExchangeIds = [
          '',
          'invalid',
          'BINANCE', // Case sensitive
          'binance-us', // Different format
          'unknown-exchange',
        ]

        for (const exchangeId of invalidExchangeIds) {
          const result = Effect.runSync(
            Effect.either(
              Schema.decode(ExchangeIdSchema)(exchangeId as ExchangeId),
            ),
          )
          expect(Either.isLeft(result)).toBe(true)
        }
      })
    })

    describe('When accessing ExchangeIdSchemaValues', () => {
      it('should contain all valid exchange ID literals', () => {
        /* Test that ExchangeIdSchemaValues contains all valid exchange ID literals */
        const expectedExchangeIds = [
          'binance',
          'coinbase',
          'kraken',
          'kucoin',
          'bybit',
          'okx',
        ]

        for (const exchangeId of expectedExchangeIds) {
          expect(ExchangeIdSchemaValues).toContain(exchangeId)
        }
      })

      it('should have the correct number of values', () => {
        /* Test that ExchangeIdSchemaValues has the correct number of values */
        // The number should match the number of Schema.Literal entries in ExchangeIdSchema
        expect(ExchangeIdSchemaValues.length).toBeGreaterThan(100) // There are over 100 exchanges
      })
    })
  })

  describe('When using validateExchangeId', () => {
    describe('When validating valid exchange IDs', () => {
      it('should return the validated exchange ID', () => {
        /* Test that validateExchangeId returns the validated exchange ID for valid inputs */
        const validExchangeIds: Array<ExchangeId> = [
          'binance',
          'coinbase',
          'kraken',
          'kucoin',
          'bybit',
          'okx',
        ]

        for (const exchangeId of validExchangeIds) {
          const result = Effect.runSync(
            Effect.either(validateExchangeId(exchangeId)),
          )
          expect(Either.isRight(result)).toBe(true)
          if (Either.isRight(result)) {
            expect(result.right).toBe(exchangeId)
          }
        }
      })
    })

    describe('When validating invalid exchange IDs', () => {
      it('should return a ParseError', () => {
        /* Test that validateExchangeId returns a ParseError for invalid inputs */
        const invalidExchangeIds = [
          '',
          'invalid',
          'BINANCE', // Case sensitive
          'binance-us', // Different format
          'unknown-exchange',
        ]

        for (const exchangeId of invalidExchangeIds) {
          const result = Effect.runSync(
            Effect.either(validateExchangeId(exchangeId)),
          )
          expect(Either.isLeft(result)).toBe(true)
        }
      })
    })
  })

  describe('When using createExchangeId', () => {
    describe('When creating from valid exchange IDs', () => {
      it('should return the validated exchange ID', () => {
        /* Test that createExchangeId returns the validated exchange ID for valid inputs */
        const validExchangeIds: Array<ExchangeId> = [
          'binance',
          'coinbase',
          'kraken',
          'kucoin',
          'bybit',
          'okx',
        ]

        for (const exchangeId of validExchangeIds) {
          const result = Effect.runSync(
            Effect.either(createExchangeId(exchangeId)),
          )
          expect(Either.isRight(result)).toBe(true)
          if (Either.isRight(result)) {
            expect(result.right).toBe(exchangeId)
          }
        }
      })
    })

    describe('When creating from invalid exchange IDs', () => {
      it('should return an InvalidExchangeIdError for empty exchange IDs', () => {
        /* Test that createExchangeId returns an InvalidExchangeIdError for empty exchange IDs */
        const result = Effect.runSync(Effect.either(createExchangeId('')))
        expect(Either.isLeft(result)).toBe(true)
        if (Either.isLeft(result)) {
          expect(result.left).toBeInstanceOf(InvalidExchangeIdError)
        }
      })

      it('should return an InvalidExchangeIdError for invalid exchange IDs', () => {
        /* Test that createExchangeId returns an InvalidExchangeIdError for invalid exchange IDs */
        const invalidExchangeIds = [
          'invalid',
          'BINANCE', // Case sensitive
          'binance-us', // Different format
          'unknown-exchange',
        ]

        for (const exchangeId of invalidExchangeIds) {
          const result = Effect.runSync(
            Effect.either(createExchangeId(exchangeId)),
          )
          expect(Either.isLeft(result)).toBe(true)
          if (Either.isLeft(result)) {
            expect(result.left).toBeInstanceOf(InvalidExchangeIdError)
          }
        }
      })
    })
  })

  describe('When using isValidExchangeId', () => {
    describe('When checking valid exchange IDs', () => {
      it('should return true', () => {
        /* Test that isValidExchangeId returns true for valid exchange IDs */
        const validExchangeIds = [
          'binance',
          'coinbase',
          'kraken',
          'kucoin',
          'bybit',
          'okx',
        ]

        for (const exchangeId of validExchangeIds) {
          const result = Effect.runSync(isValidExchangeId(exchangeId))
          expect(result).toBe(true)
        }
      })
    })

    describe('When checking invalid exchange IDs', () => {
      it('should return false', () => {
        /* Test that isValidExchangeId returns false for invalid exchange IDs */
        const invalidExchangeIds = [
          '',
          'invalid',
          'BINANCE', // Case sensitive
          'binance-us', // Different format
          'unknown-exchange',
        ]

        for (const exchangeId of invalidExchangeIds) {
          const result = Effect.runSync(isValidExchangeId(exchangeId))
          expect(result).toBe(false)
        }
      })
    })
  })

  describe('When using getExchangeDisplayName', () => {
    describe('When getting display names for exchanges with custom mappings', () => {
      it('should return the custom display name', () => {
        /* Test that getExchangeDisplayName returns the custom display name for exchanges with mappings */
        const testCases = [
          { exchangeId: 'binance', expected: 'Binance' },
          { exchangeId: 'binanceus', expected: 'Binance US' },
          { exchangeId: 'binanceusdm', expected: 'Binance USDⓈ-M' },
          { exchangeId: 'coinbase', expected: 'Coinbase' },
          { exchangeId: 'kraken', expected: 'Kraken' },
          { exchangeId: 'kucoin', expected: 'KuCoin' },
        ]

        for (const { exchangeId, expected } of testCases) {
          const result = Effect.runSync(
            getExchangeDisplayName(exchangeId as ExchangeId),
          )
          expect(result).toBe(expected)
        }
      })
    })

    describe('When getting display names for exchanges without custom mappings', () => {
      it('should capitalize the first letter', () => {
        /* Test that getExchangeDisplayName capitalizes the first letter for exchanges without mappings */
        const testCases = [
          { exchangeId: 'bitfinex', expected: 'Bitfinex' },
          { exchangeId: 'bitstamp', expected: 'Bitstamp' },
          { exchangeId: 'huobi', expected: 'Huobi' },
          { exchangeId: 'gemini', expected: 'Gemini' },
        ]

        for (const { exchangeId, expected } of testCases) {
          const result = Effect.runSync(
            getExchangeDisplayName(exchangeId as ExchangeId),
          )
          expect(result).toBe(expected)
        }
      })
    })
  })

  describe('When working with the ExchangeId type', () => {
    describe('When using it as a type constraint', () => {
      it('should allow valid exchange ID values', () => {
        /* Test that valid exchange ID values can be used where ExchangeId type is expected */
        const validExchangeIds: ExchangeId[] = [
          'binance',
          'coinbase',
          'kraken',
          'kucoin',
          'bybit',
          'okx',
        ]

        // This test passes at compile time if the type constraint works correctly
        expect(validExchangeIds.length).toBe(6)
      })

      it('should not allow invalid exchange ID values', () => {
        /* Test that invalid exchange ID values cannot be used where ExchangeId type is expected */
        // This would fail at compile time:
        // const invalidExchangeIds: ExchangeId[] = ['invalid', 'BINANCE', 'unknown-exchange']

        // Instead, we can test that the Schema rejects invalid values
        const invalidExchangeId = 'invalid'
        const result = Effect.runSync(
          Effect.either(
            Schema.decode(ExchangeIdSchema)(invalidExchangeId as ExchangeId),
          ),
        )
        expect(Either.isLeft(result)).toBe(true)
      })
    })
  })

  describe('When working with Effect results', () => {
    describe('When handling successful operations', () => {
      it('should be able to extract values from successful Effects', () => {
        /* Test that values can be extracted from successful Effects */
        const effect = validateExchangeId('binance')

        // Using runSync to extract the value
        const value = Effect.runSync(Effect.orDie(effect))
        expect(value).toBe('binance')

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: () => 'error',
          onSuccess: (value) => value,
        })

        expect(Effect.runSync(result)).toBe('binance')
      })
    })

    describe('When handling failed operations', () => {
      it('should be able to catch and handle errors from failed Effects', () => {
        /* Test that errors can be caught and handled from failed Effects */
        const effect = validateExchangeId('invalid')

        // Using runSyncEither to get Either result
        const either = Effect.runSync(Effect.either(effect))
        expect(Either.isLeft(either)).toBe(true)

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: () => 'error-handled',
          onSuccess: () => 'success',
        })

        expect(Effect.runSync(result)).toBe('error-handled')
      })
    })
  })
})
