import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import {
  InvalidPriceRelationshipError,
  InvalidTimestampError,
  InvalidVolumeError,
  createCandlestick,
  fromCCXT,
} from './Candlestick'

describe('When creating a candlestick', () => {
  describe('When validating timestamp', () => {
    it('should reject negative timestamps', () => {
      const result = createCandlestick({
        timestamp: -1,
        open: 100,
        high: 110,
        low: 90,
        close: 105,
        volume: 1000,
      })

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.left(
          new InvalidTimestampError({
            message: 'Invalid timestamp in candlestick: timestamp=-1',
          }),
        ),
      )
    })

    it('should reject zero timestamps', () => {
      const result = createCandlestick({
        timestamp: 0,
        open: 100,
        high: 110,
        low: 90,
        close: 105,
        volume: 1000,
      })

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.left(
          new InvalidTimestampError({
            message: 'Invalid timestamp in candlestick: timestamp=0',
          }),
        ),
      )
    })

    it('should accept positive timestamps', () => {
      const result = createCandlestick({
        timestamp: 1625097600000,
        open: 100,
        high: 110,
        low: 90,
        close: 105,
        volume: 1000,
      })

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.right({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 90,
          close: 105,
          volume: 1000,
        }),
      )
    })
  })

  describe('When validating price relationships', () => {
    describe('When checking high price relationships', () => {
      it('should reject when high is less than open', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 100,
          high: 90, // High less than open
          low: 80,
          close: 85,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.left(
            new InvalidPriceRelationshipError({
              message:
                'Invalid price relationship in candlestick: high=90, low=80, open=100, close=85',
            }),
          ),
        )
      })

      it('should reject when high is less than close', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 80,
          high: 90, // High less than close
          low: 80,
          close: 95,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.left(
            new InvalidPriceRelationshipError({
              message:
                'Invalid price relationship in candlestick: high=90, low=80, open=80, close=95',
            }),
          ),
        )
      })

      it('should accept when high equals open', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 100,
          high: 100, // High equals open
          low: 90,
          close: 95,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 100,
            high: 100,
            low: 90,
            close: 95,
            volume: 1000,
          }),
        )
      })

      it('should accept when high equals close', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 90,
          high: 100, // High equals close
          low: 90,
          close: 100,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 90,
            high: 100,
            low: 90,
            close: 100,
            volume: 1000,
          }),
        )
      })

      it('should accept when high is greater than both open and close', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 90,
          high: 110, // High greater than both
          low: 85,
          close: 100,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 90,
            high: 110,
            low: 85,
            close: 100,
            volume: 1000,
          }),
        )
      })
    })

    describe('When checking low price relationships', () => {
      it('should reject when low is greater than open', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 90,
          high: 110,
          low: 95, // Low greater than open
          close: 100,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.left(
            new InvalidPriceRelationshipError({
              message:
                'Invalid price relationship in candlestick: high=110, low=95, open=90, close=100',
            }),
          ),
        )
      })

      it('should reject when low is greater than close', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 98, // Low greater than close
          close: 95,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.left(
            new InvalidPriceRelationshipError({
              message:
                'Invalid price relationship in candlestick: high=110, low=98, open=100, close=95',
            }),
          ),
        )
      })

      it('should accept when low equals open', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 90,
          high: 110,
          low: 90, // Low equals open
          close: 100,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 90,
            high: 110,
            low: 90,
            close: 100,
            volume: 1000,
          }),
        )
      })

      it('should accept when low equals close', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 95, // Low equals close
          close: 95,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 100,
            high: 110,
            low: 95,
            close: 95,
            volume: 1000,
          }),
        )
      })

      it('should accept when low is less than both open and close', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 85, // Low less than both
          close: 95,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 100,
            high: 110,
            low: 85,
            close: 95,
            volume: 1000,
          }),
        )
      })
    })

    describe('When checking high-low relationship', () => {
      it('should reject when high is less than low', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 95,
          high: 90, // High less than low
          low: 95,
          close: 98,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.left(
            new InvalidPriceRelationshipError({
              message:
                'Invalid price relationship in candlestick: high=90, low=95, open=95, close=98',
            }),
          ),
        )
      })

      it('should accept when high equals low', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 100,
          high: 100, // High equals low (no price movement)
          low: 100,
          close: 100,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 100,
            high: 100,
            low: 100,
            close: 100,
            volume: 1000,
          }),
        )
      })

      it('should accept when high is greater than low', () => {
        const result = createCandlestick({
          timestamp: 1625097600000,
          open: 95,
          high: 110,
          low: 90,
          close: 100,
          volume: 1000,
        })

        expect(Effect.runSync(Effect.either(result))).toEqual(
          Either.right({
            timestamp: 1625097600000,
            open: 95,
            high: 110,
            low: 90,
            close: 100,
            volume: 1000,
          }),
        )
      })
    })
  })

  describe('When validating volume', () => {
    it('should reject negative volume', () => {
      const result = createCandlestick({
        timestamp: 1625097600000,
        open: 100,
        high: 110,
        low: 90,
        close: 105,
        volume: -1000, // Negative volume
      })

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.left(
          new InvalidVolumeError({
            message: 'Volume must be non-negative',
          }),
        ),
      )
    })

    it('should accept zero volume', () => {
      const result = createCandlestick({
        timestamp: 1625097600000,
        open: 100,
        high: 110,
        low: 90,
        close: 105,
        volume: 0, // Zero volume
      })

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.right({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 90,
          close: 105,
          volume: 0,
        }),
      )
    })

    it('should accept positive volume', () => {
      const result = createCandlestick({
        timestamp: 1625097600000,
        open: 100,
        high: 110,
        low: 90,
        close: 105,
        volume: 1000, // Positive volume
      })

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.right({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 90,
          close: 105,
          volume: 1000,
        }),
      )
    })
  })

  describe('When all validations pass', () => {
    it('should create a valid candlestick', () => {
      const result = createCandlestick({
        timestamp: 1625097600000,
        open: 100,
        high: 110,
        low: 90,
        close: 105,
        volume: 1000,
      })

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.right({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 90,
          close: 105,
          volume: 1000,
        }),
      )
    })
  })
})

describe('When creating a candlestick from CCXT format', () => {
  describe('When validating CCXT array structure', () => {
    it('should convert valid CCXT array to candlestick', () => {
      const result = fromCCXT([
        1625097600000, // timestamp
        100, // open
        110, // high
        90, // low
        105, // close
        1000, // volume
      ])

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.right({
          timestamp: 1625097600000,
          open: 100,
          high: 110,
          low: 90,
          close: 105,
          volume: 1000,
        }),
      )
    })
  })

  describe('When applying candlestick validations', () => {
    it('should apply same validation rules as direct creation', () => {
      const result = fromCCXT([
        1625097600000,
        100,
        90, // Invalid: high less than open
        85,
        95,
        1000,
      ])

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.left(
          new InvalidPriceRelationshipError({
            message:
              'Invalid price relationship in candlestick: high=90, low=85, open=100, close=95',
          }),
        ),
      )
    })
  })

  describe('When handling error cases', () => {
    it('should return InvalidPriceRelationshipError for invalid prices', () => {
      const result = fromCCXT([
        1625097600000,
        100,
        95, // high
        98, // Invalid: low > high
        105,
        1000,
      ])

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.left(
          new InvalidPriceRelationshipError({
            message:
              'Invalid price relationship in candlestick: high=95, low=98, open=100, close=105',
          }),
        ),
      )
    })

    it('should return InvalidVolumeError for invalid volume', () => {
      const result = fromCCXT([
        1625097600000,
        100,
        100, // High equals low to pass price relationship check
        100,
        100,
        -1000, // Invalid: negative volume
      ])

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.left(
          new InvalidVolumeError({
            message: 'Volume must be non-negative',
          }),
        ),
      )
    })

    it('should return InvalidTimestampError for non-positive timestamp', () => {
      const result = fromCCXT([
        0, // Invalid: zero timestamp
        100,
        110,
        90,
        105,
        1000,
      ])

      expect(Effect.runSync(Effect.either(result))).toEqual(
        Either.left(
          new InvalidTimestampError({
            message: 'Invalid timestamp in candlestick: timestamp=0',
          }),
        ),
      )
    })
  })
})
