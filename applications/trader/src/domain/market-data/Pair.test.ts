import { describe, expect, it } from 'bun:test'
import { Effect, Either, Schema } from 'effect'
import type { Pair } from './Pair'
import {
  InvalidPairError,
  PairSchema,
  fromCurrencies,
  getBaseCurrency,
  getQuoteCurrency,
  isValidPairFormat,
  validatePair,
} from './Pair'

describe('Pair', () => {
  describe('When working with the PairSchema', () => {
    describe('When validating pair values', () => {
      it('should accept valid pair values', () => {
        /* Test that valid pair values are accepted */
        const validPairs = ['BTC/USDT', 'ETH/BTC', 'SOL/USD', 'XRP/ETH']

        for (const pair of validPairs) {
          const result = Effect.runSync(
            Effect.either(Schema.decode(PairSchema)(pair)),
          )
          expect(Either.isRight(result)).toBe(true)
        }
      })

      it('should reject invalid pair values', () => {
        /* Test that invalid pair values are rejected */
        const invalidPairs = [
          '',
          'BTCUSDT',
          'BTC-USDT',
          'BTC/USDT/ETH',
          'BTC/',
          '/USDT',
          'BTC USDT',
        ]

        for (const pair of invalidPairs) {
          const result = Effect.runSync(
            Effect.either(Schema.decode(PairSchema)(pair)),
          )
          expect(Either.isLeft(result)).toBe(true)
        }
      })
    })
  })

  describe('When using validatePair', () => {
    describe('When validating valid pairs', () => {
      it('should return the validated pair', () => {
        /* Test that validatePair returns the validated pair for valid inputs */
        const validPairs = ['BTC/USDT', 'ETH/BTC', 'SOL/USD', 'XRP/ETH']

        for (const pair of validPairs) {
          const result = Effect.runSync(Effect.either(validatePair(pair)))
          expect(Either.isRight(result)).toBe(true)
          if (Either.isRight(result)) {
            expect(result.right).toBe(pair)
          }
        }
      })
    })

    describe('When validating invalid pairs', () => {
      it('should return a ParseError', () => {
        /* Test that validatePair returns a ParseError for invalid inputs */
        const invalidPairs = [
          '',
          'BTCUSDT',
          'BTC-USDT',
          'BTC/USDT/ETH',
          'BTC/',
          '/USDT',
          'BTC USDT',
        ]

        for (const pair of invalidPairs) {
          const result = Effect.runSync(Effect.either(validatePair(pair)))
          expect(Either.isLeft(result)).toBe(true)
        }
      })
    })
  })

  describe('When using isValidPairFormat', () => {
    describe('When checking valid pair formats', () => {
      it('should return true', () => {
        /* Test that isValidPairFormat returns true for valid formats */
        const validFormats = ['BTC/USDT', 'ETH/BTC', 'SOL/USD', 'XRP/ETH']

        for (const format of validFormats) {
          const result = Effect.runSync(isValidPairFormat(format))
          expect(result).toBe(true)
        }
      })
    })

    describe('When checking invalid pair formats', () => {
      it('should return false', () => {
        /* Test that isValidPairFormat returns false for invalid formats */
        const invalidFormats = [
          '',
          'BTCUSDT',
          'BTC-USDT',
          'BTC/USDT/ETH',
          'BTC/',
          '/USDT',
          'BTC USDT',
        ]

        for (const format of invalidFormats) {
          const result = Effect.runSync(isValidPairFormat(format))
          expect(result).toBe(false)
        }
      })
    })
  })

  describe('When using getBaseCurrency', () => {
    describe('When extracting from valid pairs', () => {
      it('should return the base currency', () => {
        /* Test that getBaseCurrency returns the base currency for valid pairs */
        const testCases = [
          { pair: 'BTC/USDT', expected: 'BTC' },
          { pair: 'ETH/BTC', expected: 'ETH' },
          { pair: 'SOL/USD', expected: 'SOL' },
          { pair: 'XRP/ETH', expected: 'XRP' },
        ]

        for (const { pair, expected } of testCases) {
          const result = Effect.runSync(getBaseCurrency(pair as Pair))
          expect(result).toBe(expected)
        }
      })
    })

    describe('When extracting from invalid pairs', () => {
      it('should return an InvalidPairError', () => {
        /* Test that getBaseCurrency returns an InvalidPairError for invalid pairs */
        // This test is a bit artificial since the type system should prevent invalid pairs
        // But we can test the error handling logic
        const invalidPair = 'BTC' as Pair // Force cast to bypass type checking

        const result = Effect.runSync(
          Effect.either(getBaseCurrency(invalidPair)),
        )
        expect(Either.isLeft(result)).toBe(true)
        if (Either.isLeft(result)) {
          expect(result.left).toBeInstanceOf(InvalidPairError)
        }
      })
    })
  })

  describe('When using getQuoteCurrency', () => {
    describe('When extracting from valid pairs', () => {
      it('should return the quote currency', () => {
        /* Test that getQuoteCurrency returns the quote currency for valid pairs */
        const testCases = [
          { pair: 'BTC/USDT', expected: 'USDT' },
          { pair: 'ETH/BTC', expected: 'BTC' },
          { pair: 'SOL/USD', expected: 'USD' },
          { pair: 'XRP/ETH', expected: 'ETH' },
        ]

        for (const { pair, expected } of testCases) {
          const result = Effect.runSync(getQuoteCurrency(pair as Pair))
          expect(result).toBe(expected)
        }
      })
    })

    describe('When extracting from invalid pairs', () => {
      it('should return an InvalidPairError', () => {
        /* Test that getQuoteCurrency returns an InvalidPairError for invalid pairs */
        // This test is a bit artificial since the type system should prevent invalid pairs
        // But we can test the error handling logic
        const invalidPair = 'USDT' as Pair // Force cast to bypass type checking

        const result = Effect.runSync(
          Effect.either(getQuoteCurrency(invalidPair)),
        )
        expect(Either.isLeft(result)).toBe(true)
        if (Either.isLeft(result)) {
          expect(result.left).toBeInstanceOf(InvalidPairError)
        }
      })
    })
  })

  describe('When using fromCurrencies', () => {
    describe('When creating from valid currencies', () => {
      it('should return the validated pair', () => {
        /* Test that fromCurrencies returns the validated pair for valid inputs */
        const testCases = [
          { base: 'BTC', quote: 'USDT', expected: 'BTC/USDT' },
          { base: 'ETH', quote: 'BTC', expected: 'ETH/BTC' },
          { base: 'SOL', quote: 'USD', expected: 'SOL/USD' },
          { base: 'XRP', quote: 'ETH', expected: 'XRP/ETH' },
        ]

        for (const { base, quote, expected } of testCases) {
          const result = Effect.runSync(
            Effect.either(fromCurrencies(base, quote)),
          )
          expect(Either.isRight(result)).toBe(true)
          if (Either.isRight(result)) {
            expect(result.right).toBe(expected)
          }
        }
      })
    })

    describe('When creating from invalid currencies', () => {
      it('should return an InvalidPairError for empty currencies', () => {
        /* Test that fromCurrencies returns an InvalidPairError for empty currencies */
        const testCases = [
          { base: '', quote: 'USDT' },
          { base: 'BTC', quote: '' },
          { base: '', quote: '' },
        ]

        for (const { base, quote } of testCases) {
          const result = Effect.runSync(
            Effect.either(fromCurrencies(base, quote)),
          )
          expect(Either.isLeft(result)).toBe(true)
          if (Either.isLeft(result)) {
            expect(result.left).toBeInstanceOf(InvalidPairError)
          }
        }
      })

      it('should return a ParseError for invalid pair format', () => {
        /* Test that fromCurrencies returns a ParseError for invalid pair format */
        // This test is a bit artificial since our implementation should always create valid formats
        // But we can test the validation logic
        const result = Effect.runSync(
          Effect.either(fromCurrencies('BTC USDT', 'invalid')),
        )
        expect(Either.isLeft(result)).toBe(true)
      })
    })
  })

  describe('When working with the Pair type', () => {
    describe('When using it as a type constraint', () => {
      it('should allow valid pair values', () => {
        /* Test that valid pair values can be used where Pair type is expected */
        // This is more of a compile-time check, but we can verify the schema accepts these values
        const validPairs = ['BTC/USDT', 'ETH/BTC', 'SOL/USD', 'XRP/ETH']

        for (const pair of validPairs) {
          const result = Effect.runSync(
            Effect.either(Schema.decode(PairSchema)(pair)),
          )
          expect(Either.isRight(result)).toBe(true)
        }
      })
    })
  })

  describe('When working with Effect results', () => {
    describe('When handling successful operations', () => {
      it('should be able to extract values from successful Effects', () => {
        /* Test that values can be extracted from successful Effects */
        const effect = getBaseCurrency('BTC/USDT' as Pair)

        // Using runSync to extract the value
        const value = Effect.runSync(effect)
        expect(value).toBe('BTC')

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: () => 'error',
          onSuccess: (value) => value,
        })

        expect(Effect.runSync(result)).toBe('BTC')
      })
    })

    describe('When handling failed operations', () => {
      it('should be able to catch and handle errors from failed Effects', () => {
        /* Test that errors can be caught and handled from failed Effects */
        const invalidPair = 'BTC' as Pair // Force cast to bypass type checking
        const effect = getBaseCurrency(invalidPair)

        // Using runSyncEither to get Either result
        const either = Effect.runSync(Effect.either(effect))
        expect(Either.isLeft(either)).toBe(true)

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: (error) => {
            expect(error).toBeInstanceOf(InvalidPairError)
            return 'error-handled'
          },
          onSuccess: () => 'success',
        })

        expect(Effect.runSync(result)).toBe('error-handled')
      })
    })
  })
})
