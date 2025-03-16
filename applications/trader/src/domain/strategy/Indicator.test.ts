import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import type { Candlestick } from '../market-data/Candlestick'
import {
  InvalidIndicatorParametersError,
  PriceType,
  createIndicator,
  extractPrices,
  getPriceValue,
  validateCandlestickLength,
} from './Indicator'

describe('Indicator', () => {
  // Sample candlestick for testing
  const sampleCandlestick: Candlestick = {
    timestamp: 1609459200000, // 2021-01-01
    open: 100,
    high: 110,
    low: 90,
    close: 105,
    volume: 1000,
  }

  // Sample candlestick array for testing
  const sampleCandlesticks: Candlestick[] = [
    {
      timestamp: 1609459200000, // 2021-01-01
      open: 100,
      high: 110,
      low: 90,
      close: 105,
      volume: 1000,
    },
    {
      timestamp: 1609545600000, // 2021-01-02
      open: 105,
      high: 115,
      low: 100,
      close: 110,
      volume: 1200,
    },
    {
      timestamp: 1609632000000, // 2021-01-03
      open: 110,
      high: 120,
      low: 105,
      close: 115,
      volume: 1500,
    },
  ]

  describe('getPriceValue', () => {
    it('should return the close price by default', () => {
      const result = getPriceValue(sampleCandlestick)
      expect(result).toBe(105)
    })

    it('should return the open price when specified', () => {
      const result = getPriceValue(sampleCandlestick, PriceType.Open)
      expect(result).toBe(100)
    })

    it('should return the high price when specified', () => {
      const result = getPriceValue(sampleCandlestick, PriceType.High)
      expect(result).toBe(110)
    })

    it('should return the low price when specified', () => {
      const result = getPriceValue(sampleCandlestick, PriceType.Low)
      expect(result).toBe(90)
    })

    it('should return the typical price when specified', () => {
      const result = getPriceValue(sampleCandlestick, PriceType.Typical)
      expect(result).toBe((110 + 90 + 105) / 3)
    })

    it('should return the median price when specified', () => {
      const result = getPriceValue(sampleCandlestick, PriceType.Median)
      expect(result).toBe((110 + 90) / 2)
    })

    it('should return the volume when specified', () => {
      const result = getPriceValue(sampleCandlestick, PriceType.Volume)
      expect(result).toBe(1000)
    })

    it('should return the close price for unknown price types', () => {
      const result = getPriceValue(sampleCandlestick, 'unknown' as PriceType)
      expect(result).toBe(105)
    })
  })

  describe('createIndicator', () => {
    it('should create a valid indicator', () => {
      const calculateFn = (candlesticks: Candlestick[]) =>
        Effect.succeed(candlesticks.length)

      const result = Effect.runSync(
        Effect.either(
          createIndicator(
            'Test Indicator',
            'A test indicator',
            { period: 14 },
            calculateFn,
          ),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.name).toBe('Test Indicator')
        expect(result.right.description).toBe('A test indicator')
        expect(result.right.parameters).toEqual({ period: 14 })

        // Test the calculate function
        const calcResult = Effect.runSync(
          result.right.calculate(sampleCandlesticks),
        )
        expect(calcResult).toBe(3) // Length of sampleCandlesticks
      }
    })

    it('should fail with empty name', () => {
      const calculateFn = (candlesticks: Candlestick[]) =>
        Effect.succeed(candlesticks.length)

      const result = Effect.runSync(
        Effect.either(
          createIndicator('', 'A test indicator', { period: 14 }, calculateFn),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidIndicatorParametersError)
        expect((result.left as InvalidIndicatorParametersError).message).toBe(
          'Indicator name cannot be empty',
        )
      }
    })

    it('should fail with invalid period', () => {
      const calculateFn = (candlesticks: Candlestick[]) =>
        Effect.succeed(candlesticks.length)

      const result = Effect.runSync(
        Effect.either(
          createIndicator(
            'Test Indicator',
            'A test indicator',
            { period: -1 },
            calculateFn,
          ),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidIndicatorParametersError)
        expect((result.left as InvalidIndicatorParametersError).message).toBe(
          'Period must be a positive number',
        )
      }
    })

    it('should fail with non-numeric period', () => {
      const calculateFn = (candlesticks: Candlestick[]) =>
        Effect.succeed(candlesticks.length)

      const result = Effect.runSync(
        Effect.either(
          createIndicator(
            'Test Indicator',
            'A test indicator',
            { period: 'invalid' as unknown as number },
            calculateFn,
          ),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidIndicatorParametersError)
        expect((result.left as InvalidIndicatorParametersError).message).toBe(
          'Period must be a positive number',
        )
      }
    })
  })

  describe('validateCandlestickLength', () => {
    it('should succeed when there are enough candlesticks', () => {
      const result = Effect.runSync(
        Effect.either(validateCandlestickLength(sampleCandlesticks, 3)),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual(sampleCandlesticks)
      }
    })

    it('should fail when there are not enough candlesticks', () => {
      const result = Effect.runSync(
        Effect.either(validateCandlestickLength(sampleCandlesticks, 5)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidIndicatorParametersError)
        expect((result.left as InvalidIndicatorParametersError).message).toBe(
          'Not enough data: need at least 5 candlesticks, but got 3',
        )
      }
    })
  })

  describe('extractPrices', () => {
    it('should extract close prices by default', () => {
      const result = extractPrices(sampleCandlesticks)
      expect(result).toEqual([105, 110, 115])
    })

    it('should extract open prices when specified', () => {
      const result = extractPrices(sampleCandlesticks, PriceType.Open)
      expect(result).toEqual([100, 105, 110])
    })

    it('should extract high prices when specified', () => {
      const result = extractPrices(sampleCandlesticks, PriceType.High)
      expect(result).toEqual([110, 115, 120])
    })

    it('should extract low prices when specified', () => {
      const result = extractPrices(sampleCandlesticks, PriceType.Low)
      expect(result).toEqual([90, 100, 105])
    })

    it('should extract typical prices when specified', () => {
      const result = extractPrices(sampleCandlesticks, PriceType.Typical)
      expect(result).toEqual([
        (110 + 90 + 105) / 3,
        (115 + 100 + 110) / 3,
        (120 + 105 + 115) / 3,
      ])
    })

    it('should extract median prices when specified', () => {
      const result = extractPrices(sampleCandlesticks, PriceType.Median)
      expect(result).toEqual([(110 + 90) / 2, (115 + 100) / 2, (120 + 105) / 2])
    })

    it('should extract volumes when specified', () => {
      const result = extractPrices(sampleCandlesticks, PriceType.Volume)
      expect(result).toEqual([1000, 1200, 1500])
    })
  })
})
