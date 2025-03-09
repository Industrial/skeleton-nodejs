import { describe, expect, it } from 'bun:test'
import { Effect, Either, Schema } from 'effect'
import type { Timeframe } from './Timeframe'
import {
  InvalidTimeframeMillisecondsError,
  TimeframeSchema,
  TimeframeSchemaValues,
  fromMilliseconds,
  toMilliseconds,
} from './Timeframe'

describe('Timeframe', () => {
  describe('When working with the TimeframeSchema', () => {
    describe('When validating timeframe values', () => {
      it('should accept valid timeframe values', () => {
        /* Test that valid timeframe values are accepted */
        const validTimeframes = ['1m', '3m', '5m', '15m', '1h', '4h', '1d']

        for (const timeframe of validTimeframes) {
          const result = Effect.runSync(
            Effect.either(
              Schema.decode(TimeframeSchema)(timeframe as Timeframe),
            ),
          )
          expect(Either.isRight(result)).toBe(true)
        }
      })

      it('should reject invalid timeframe values', () => {
        /* Test that invalid timeframe values are rejected */
        const invalidTimeframes = ['2m', '10m', '2h', '12h', '2d', 'invalid']

        for (const timeframe of invalidTimeframes) {
          const result = Effect.runSync(
            Effect.either(
              Schema.decode(TimeframeSchema)(timeframe as Timeframe),
            ),
          )
          expect(Either.isLeft(result)).toBe(true)
        }
      })
    })

    describe('When accessing TimeframeSchemaValues', () => {
      it('should contain all valid timeframe literals', () => {
        /* Test that TimeframeSchemaValues contains all valid timeframe literals */
        const expectedTimeframes = ['1m', '3m', '5m', '15m', '1h', '4h', '1d']

        for (const timeframe of expectedTimeframes) {
          expect(TimeframeSchemaValues).toContain(timeframe)
        }
      })

      it('should have the correct number of values', () => {
        /* Test that TimeframeSchemaValues has the correct number of values */
        expect(TimeframeSchemaValues.length).toBe(7)
      })
    })
  })

  describe('When converting timeframes to milliseconds', () => {
    describe('When using toMilliseconds with valid inputs', () => {
      describe('When converting 1m timeframe', () => {
        it('should return 60000 milliseconds', () => {
          /* Test that 1m converts to 60000 milliseconds */
          const result = Effect.runSync(toMilliseconds('1m'))
          expect(result).toBe(60 * 1000)
        })
      })

      describe('When converting 3m timeframe', () => {
        it('should return 180000 milliseconds', () => {
          /* Test that 3m converts to 180000 milliseconds */
          const result = Effect.runSync(toMilliseconds('3m'))
          expect(result).toBe(3 * 60 * 1000)
        })
      })

      describe('When converting 5m timeframe', () => {
        it('should return 300000 milliseconds', () => {
          /* Test that 5m converts to 300000 milliseconds */
          const result = Effect.runSync(toMilliseconds('5m'))
          expect(result).toBe(5 * 60 * 1000)
        })
      })

      describe('When converting 15m timeframe', () => {
        it('should return 900000 milliseconds', () => {
          /* Test that 15m converts to 900000 milliseconds */
          const result = Effect.runSync(toMilliseconds('15m'))
          expect(result).toBe(15 * 60 * 1000)
        })
      })

      describe('When converting 1h timeframe', () => {
        it('should return 3600000 milliseconds', () => {
          /* Test that 1h converts to 3600000 milliseconds */
          const result = Effect.runSync(toMilliseconds('1h'))
          expect(result).toBe(60 * 60 * 1000)
        })
      })

      describe('When converting 4h timeframe', () => {
        it('should return 14400000 milliseconds', () => {
          /* Test that 4h converts to 14400000 milliseconds */
          const result = Effect.runSync(toMilliseconds('4h'))
          expect(result).toBe(4 * 60 * 60 * 1000)
        })
      })

      describe('When converting 1d timeframe', () => {
        it('should return 86400000 milliseconds', () => {
          /* Test that 1d converts to 86400000 milliseconds */
          const result = Effect.runSync(toMilliseconds('1d'))
          expect(result).toBe(24 * 60 * 60 * 1000)
        })
      })
    })

    describe('When using toMilliseconds with invalid inputs', () => {
      it('should return a ParseError', () => {
        /* Test that invalid timeframe values return a ParseError */
        const result = Effect.runSync(
          Effect.either(toMilliseconds('invalid' as Timeframe)),
        )
        expect(Either.isLeft(result)).toBe(true)
      })
    })
  })

  describe('When converting milliseconds to timeframes', () => {
    describe('When using fromMilliseconds with valid inputs', () => {
      describe('When converting 60000 milliseconds', () => {
        it('should return 1m timeframe', () => {
          /* Test that 60000 milliseconds converts to 1m */
          const result = Effect.runSync(fromMilliseconds(60 * 1000))
          expect(result).toBe('1m')
        })
      })

      describe('When converting 180000 milliseconds', () => {
        it('should return 3m timeframe', () => {
          /* Test that 180000 milliseconds converts to 3m */
          const result = Effect.runSync(fromMilliseconds(3 * 60 * 1000))
          expect(result).toBe('3m')
        })
      })

      describe('When converting 300000 milliseconds', () => {
        it('should return 5m timeframe', () => {
          /* Test that 300000 milliseconds converts to 5m */
          const result = Effect.runSync(fromMilliseconds(5 * 60 * 1000))
          expect(result).toBe('5m')
        })
      })

      describe('When converting 900000 milliseconds', () => {
        it('should return 15m timeframe', () => {
          /* Test that 900000 milliseconds converts to 15m */
          const result = Effect.runSync(fromMilliseconds(15 * 60 * 1000))
          expect(result).toBe('15m')
        })
      })

      describe('When converting 3600000 milliseconds', () => {
        it('should return 1h timeframe', () => {
          /* Test that 3600000 milliseconds converts to 1h */
          const result = Effect.runSync(fromMilliseconds(60 * 60 * 1000))
          expect(result).toBe('1h')
        })
      })

      describe('When converting 14400000 milliseconds', () => {
        it('should return 4h timeframe', () => {
          /* Test that 14400000 milliseconds converts to 4h */
          const result = Effect.runSync(fromMilliseconds(4 * 60 * 60 * 1000))
          expect(result).toBe('4h')
        })
      })

      describe('When converting 86400000 milliseconds', () => {
        it('should return 1d timeframe', () => {
          /* Test that 86400000 milliseconds converts to 1d */
          const result = Effect.runSync(fromMilliseconds(24 * 60 * 60 * 1000))
          expect(result).toBe('1d')
        })
      })
    })

    describe('When using fromMilliseconds with invalid inputs', () => {
      describe('When converting a millisecond value that does not match any timeframe', () => {
        it('should return an InvalidTimeframeMillisecondsError', () => {
          /* Test that invalid millisecond values return an InvalidTimeframeMillisecondsError */
          const result = Effect.runSync(
            Effect.either(fromMilliseconds(42 * 1000)),
          )

          expect(Either.isLeft(result)).toBe(true)
          if (Either.isLeft(result)) {
            expect(result.left).toBeInstanceOf(
              InvalidTimeframeMillisecondsError,
            )
          }
        })
      })

      describe('When converting a negative millisecond value', () => {
        it('should return an InvalidTimeframeMillisecondsError', () => {
          /* Test that negative millisecond values return an InvalidTimeframeMillisecondsError */
          const result = Effect.runSync(
            Effect.either(fromMilliseconds(-60 * 1000)),
          )

          expect(Either.isLeft(result)).toBe(true)
          if (Either.isLeft(result)) {
            expect(result.left).toBeInstanceOf(
              InvalidTimeframeMillisecondsError,
            )
          }
        })
      })

      describe('When converting zero milliseconds', () => {
        it('should return an InvalidTimeframeMillisecondsError', () => {
          /* Test that zero milliseconds returns an InvalidTimeframeMillisecondsError */
          const result = Effect.runSync(Effect.either(fromMilliseconds(0)))

          expect(Either.isLeft(result)).toBe(true)
          if (Either.isLeft(result)) {
            expect(result.left).toBeInstanceOf(
              InvalidTimeframeMillisecondsError,
            )
          }
        })
      })
    })
  })

  describe('When working with the Timeframe type', () => {
    describe('When using it as a type constraint', () => {
      it('should allow valid timeframe values', () => {
        /* Test that valid timeframe values can be used where Timeframe type is expected */
        const validTimeframes: Timeframe[] = [
          '1m',
          '3m',
          '5m',
          '15m',
          '1h',
          '4h',
          '1d',
        ]

        // This test passes at compile time if the type constraint works correctly
        expect(validTimeframes.length).toBe(7)
      })

      it('should not allow invalid timeframe values', () => {
        /* Test that invalid timeframe values cannot be used where Timeframe type is expected */
        // This would fail at compile time:
        // const invalidTimeframes: Timeframe[] = ['2m', '10m', '2h', '12h', '2d', 'invalid']

        // Instead, we can test that the Schema rejects invalid values
        const invalidTimeframe = 'invalid'
        const result = Effect.runSync(
          Effect.either(
            Schema.decode(TimeframeSchema)(invalidTimeframe as Timeframe),
          ),
        )
        expect(Either.isLeft(result)).toBe(true)
      })
    })
  })

  describe('When working with Effect results', () => {
    describe('When handling successful conversions', () => {
      it('should be able to extract values from successful Effects', () => {
        /* Test that values can be extracted from successful Effects */
        const effect = toMilliseconds('1m')

        // Using runSync to extract the value
        const value = Effect.runSync(effect)
        expect(value).toBe(60 * 1000)

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: () => 'error',
          onSuccess: (value) => value,
        })

        expect(Effect.runSync(result)).toBe(60 * 1000)
      })
    })

    describe('When handling failed conversions', () => {
      it('should be able to catch and handle errors from failed Effects', () => {
        /* Test that errors can be caught and handled from failed Effects */
        const effect = fromMilliseconds(42 * 1000) // Invalid milliseconds value

        // Using runSyncEither to get Either result
        const either = Effect.runSync(Effect.either(effect))
        expect(Either.isLeft(either)).toBe(true)

        // Using match to handle both success and failure cases
        const result = Effect.match(effect, {
          onFailure: (error) => {
            expect(error).toBeInstanceOf(InvalidTimeframeMillisecondsError)
            return 'error-handled'
          },
          onSuccess: () => 'success',
        })

        expect(Effect.runSync(result)).toBe('error-handled')
      })
    })
  })
})
