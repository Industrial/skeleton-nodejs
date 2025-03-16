import { describe, expect, it } from 'bun:test'
import { Effect, Either, Schema } from 'effect'
import type { AppConfig } from './AppConfig'
import {
  AppConfigSchema,
  InvalidAppConfigError,
  createAppConfig,
  isValidAppConfig,
  validateAppConfig,
} from './AppConfig'

describe('AppConfig', () => {
  describe('When working with the AppConfigSchema', () => {
    describe('When validating AppConfig values', () => {
      it('should accept valid AppConfig values', () => {
        /* Test that valid AppConfig values are accepted */
        const validConfig = {
          exchange: 'binance',
          pair: 'BTC/USDT',
          timeframe: '1h',
        }

        const result = Effect.runSync(
          Effect.either(
            Schema.decode(AppConfigSchema)(validConfig as AppConfig),
          ),
        )
        expect(Either.isRight(result)).toBe(true)
      })

      it('should reject invalid AppConfig values', () => {
        /* Test that invalid AppConfig values are rejected */
        const invalidConfigs = [
          { exchange: 'invalid', pair: 'BTC/USDT', timeframe: '1h' },
          { exchange: 'binance', pair: '', timeframe: '1h' },
          { exchange: 'binance', pair: 'BTC/USDT', timeframe: 'invalid' },
          { exchange: 'binance', timeframe: '1h' }, // missing pair
          { exchange: 'binance', pair: 'BTC/USDT' }, // missing timeframe
          { pair: 'BTC/USDT', timeframe: '1h' }, // missing exchange
        ]

        for (const config of invalidConfigs) {
          const result = Effect.runSync(
            Effect.either(Schema.decode(AppConfigSchema)(config as AppConfig)),
          )

          expect(Either.isLeft(result)).toBe(true)
        }
      })
    })
  })

  describe('When using validateAppConfig', () => {
    describe('When validating valid AppConfig objects', () => {
      it('should return the validated AppConfig', () => {
        /* Test that validateAppConfig returns the validated AppConfig for valid inputs */
        const validConfig = {
          exchange: 'binance',
          pair: 'BTC/USDT',
          timeframe: '1h',
        }

        const result = Effect.runSync(
          Effect.either(validateAppConfig(validConfig)),
        )
        expect(Either.isRight(result)).toBe(true)
        if (Either.isRight(result)) {
          expect(result.right).toEqual(validConfig as AppConfig)
        }
      })
    })

    describe('When validating invalid AppConfig objects', () => {
      it('should return an InvalidAppConfigError', () => {
        /* Test that validateAppConfig returns an InvalidAppConfigError for invalid inputs */
        const invalidConfig = {
          exchange: 'invalid',
          pair: 'BTC/USDT',
          timeframe: '1h',
        }

        const result = Effect.runSync(
          Effect.either(validateAppConfig(invalidConfig)),
        )
        expect(Either.isLeft(result)).toBe(true)
        if (Either.isLeft(result)) {
          expect(result.left).toBeInstanceOf(InvalidAppConfigError)
        }
      })
    })
  })

  describe('When using createAppConfig', () => {
    describe('When creating from valid inputs', () => {
      it('should return the validated AppConfig', () => {
        /* Test that createAppConfig returns the validated AppConfig for valid inputs */
        const result = Effect.runSync(
          Effect.either(createAppConfig('binance', 'BTC/USDT', '1h')),
        )
        expect(Either.isRight(result)).toBe(true)
        if (Either.isRight(result)) {
          expect(result.right).toEqual({
            exchange: 'binance',
            pair: 'BTC/USDT',
            timeframe: '1h',
          })
        }
      })
    })

    describe('When creating from invalid inputs', () => {
      it('should return an InvalidAppConfigError for empty pair', () => {
        /* Test that createAppConfig returns an InvalidAppConfigError for empty pair */
        const result = Effect.runSync(
          Effect.either(createAppConfig('binance', '', '1h')),
        )
        expect(Either.isLeft(result)).toBe(true)
        if (Either.isLeft(result)) {
          expect(result.left).toBeInstanceOf(InvalidAppConfigError)
          const error = result.left as InvalidAppConfigError
          expect(error.message).toBe('Pair cannot be empty')
          expect(error.field).toBe('pair')
          expect(error.value).toBe('')
        }
      })

      it('should return an InvalidAppConfigError for invalid exchange', () => {
        /* Test that createAppConfig returns an InvalidAppConfigError for invalid exchange */
        const result = Effect.runSync(
          Effect.either(createAppConfig('invalid', 'BTC/USDT', '1h')),
        )
        expect(Either.isLeft(result)).toBe(true)
        if (Either.isLeft(result)) {
          expect(result.left).toBeInstanceOf(InvalidAppConfigError)
          const error = result.left as InvalidAppConfigError
          expect(error.message).toBe('Invalid exchange ID')
          expect(error.field).toBe('exchange')
          expect(error.value).toBe('invalid')
        }
      })

      it('should return an InvalidAppConfigError for invalid timeframe', () => {
        /* Test that createAppConfig returns an InvalidAppConfigError for invalid timeframe */
        const result = Effect.runSync(
          Effect.either(createAppConfig('binance', 'BTC/USDT', 'invalid')),
        )
        expect(Either.isLeft(result)).toBe(true)
        if (Either.isLeft(result)) {
          expect(result.left).toBeInstanceOf(InvalidAppConfigError)
          const error = result.left as InvalidAppConfigError
          expect(error.message).toBe('Invalid timeframe')
          expect(error.field).toBe('timeframe')
          expect(error.value).toBe('invalid')
        }
      })
    })
  })

  describe('When using isValidAppConfig', () => {
    describe('When checking valid AppConfig objects', () => {
      it('should return true', () => {
        /* Test that isValidAppConfig returns true for valid AppConfig objects */
        const validConfig = {
          exchange: 'binance',
          pair: 'BTC/USDT',
          timeframe: '1h',
        }

        const result = Effect.runSync(isValidAppConfig(validConfig))
        expect(result).toBe(true)
      })
    })

    describe('When checking invalid AppConfig objects', () => {
      it('should return false', () => {
        /* Test that isValidAppConfig returns false for invalid AppConfig objects */
        const invalidConfigs = [
          { exchange: 'invalid', pair: 'BTC/USDT', timeframe: '1h' },
          { exchange: 'binance', pair: '', timeframe: '1h' },
          { exchange: 'binance', pair: 'BTC/USDT', timeframe: 'invalid' },
          { exchange: 'binance', timeframe: '1h' }, // missing pair
          { exchange: 'binance', pair: 'BTC/USDT' }, // missing timeframe
          { pair: 'BTC/USDT', timeframe: '1h' }, // missing exchange
        ]

        for (const config of invalidConfigs) {
          const result = Effect.runSync(isValidAppConfig(config))
          expect(result).toBe(false)
        }
      })
    })
  })

  describe('When working with Effect results', () => {
    describe('When handling successful operations', () => {
      it('should be able to extract values from successful Effects', () => {
        /* Test that values can be extracted from successful Effects */
        const effect = createAppConfig('binance', 'BTC/USDT', '1h')

        // Using runSync to extract the value
        const value = Effect.runSync(Effect.orDie(effect))
        expect(value).toEqual({
          exchange: 'binance',
          pair: 'BTC/USDT',
          timeframe: '1h',
        })

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: () => 'error',
          onSuccess: (value) => value,
        })

        const matchResult = Effect.runSync(result)
        expect(matchResult).toEqual({
          exchange: 'binance',
          pair: 'BTC/USDT',
          timeframe: '1h',
        })
      })
    })

    describe('When handling failed operations', () => {
      it('should be able to catch and handle errors from failed Effects', () => {
        /* Test that errors can be caught and handled from failed Effects */
        const effect = createAppConfig('invalid', 'BTC/USDT', '1h')

        // Using runSyncEither to get Either result
        const either = Effect.runSync(Effect.either(effect))
        expect(Either.isLeft(either)).toBe(true)

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: (error) => {
            expect(error).toBeInstanceOf(InvalidAppConfigError)
            return 'error-handled'
          },
          onSuccess: () => 'success',
        })

        expect(Effect.runSync(result)).toBe('error-handled')
      })
    })
  })
})
