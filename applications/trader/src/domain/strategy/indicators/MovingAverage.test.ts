import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import type { Candlestick } from '../../market-data/Candlestick'
import { InvalidIndicatorParametersError, PriceType } from '../Indicator'
import {
  calculateEMA,
  calculateEMAFromCandlesticks,
  calculateSMA,
  calculateSMAFromCandlesticks,
  createExponentialMovingAverage,
  createMovingAverage,
} from './MovingAverage'

describe('MovingAverage', () => {
  // Sample data for testing
  const sampleValues = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

  // Sample candlesticks for testing
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
    {
      timestamp: 1609718400000, // 2021-01-04
      open: 115,
      high: 125,
      low: 110,
      close: 120,
      volume: 1800,
    },
    {
      timestamp: 1609804800000, // 2021-01-05
      open: 120,
      high: 130,
      low: 115,
      close: 125,
      volume: 2000,
    },
  ]

  describe('calculateSMA', () => {
    it('should calculate SMA correctly', () => {
      const result = calculateSMA(sampleValues, 3)
      expect(result).toEqual([2, 3, 4, 5, 6, 7, 8, 9])
    })

    it('should return empty array when not enough values', () => {
      const result = calculateSMA(sampleValues, 11)
      expect(result).toEqual([])
    })

    it('should handle edge case with period equal to values length', () => {
      const result = calculateSMA(sampleValues, 10)
      expect(result).toEqual([5.5])
    })
  })

  describe('calculateSMAFromCandlesticks', () => {
    it('should calculate SMA from close prices by default', () => {
      const result = Effect.runSync(
        calculateSMAFromCandlesticks(sampleCandlesticks, 3),
      )
      expect(result).toEqual([110, 115, 120])
    })

    it('should calculate SMA from specified price type', () => {
      const result = Effect.runSync(
        calculateSMAFromCandlesticks(sampleCandlesticks, 3, PriceType.Open),
      )
      expect(result).toEqual([105, 110, 115])
    })

    it('should return empty array when not enough candlesticks', () => {
      const result = Effect.runSync(
        calculateSMAFromCandlesticks(sampleCandlesticks, 6),
      )
      expect(result).toEqual([])
    })
  })

  describe('createMovingAverage', () => {
    it('should create a valid SMA indicator', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverage({
            period: 3,
            priceType: PriceType.Close,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.name).toBe('Simple Moving Average (3)')
        expect(result.right.description).toBe('Average price over 3 periods')
        expect(result.right.parameters).toEqual({
          period: 3,
          priceType: PriceType.Close,
        })

        // Test the calculate function
        const calcResult = Effect.runSync(
          result.right.calculate(sampleCandlesticks),
        )
        expect(calcResult).toEqual([110, 115, 120])
      }
    })

    it('should create a valid SMA indicator with default price type', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverage({
            period: 3,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.parameters).toEqual({
          period: 3,
          priceType: PriceType.Close,
        })
      }
    })

    it('should fail with non-positive period', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverage({
            period: 0,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidIndicatorParametersError)
        expect((result.left as InvalidIndicatorParametersError).message).toBe(
          'Period must be positive',
        )
      }
    })

    it('should handle not enough candlesticks gracefully', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverage({
            period: 10,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        // Test the calculate function with not enough candlesticks
        const calcResult = Effect.runSync(
          result.right.calculate(sampleCandlesticks),
        )
        expect(calcResult).toEqual([])
      }
    })
  })

  describe('calculateEMA', () => {
    it('should calculate EMA correctly', () => {
      const result = calculateEMA(sampleValues, 3)
      // Hand-calculated values for verification:
      // First value is SMA of first 3 values: (1+2+3)/3 = 2
      // Multiplier = 2/(3+1) = 0.5
      // Second value: (4-2)*0.5 + 2 = 3
      // Third value: (5-3)*0.5 + 3 = 4
      // And so on...
      expect(result[0]).toBeCloseTo(2)
      expect(result[1]).toBeCloseTo(3)
      expect(result[2]).toBeCloseTo(4)
      expect(result.length).toBe(8)
    })

    it('should return empty array when not enough values', () => {
      const result = calculateEMA(sampleValues, 11)
      expect(result).toEqual([])
    })

    it('should handle edge case with period equal to values length', () => {
      const result = calculateEMA(sampleValues, 10)
      expect(result).toEqual([5.5])
    })
  })

  describe('calculateEMAFromCandlesticks', () => {
    it('should calculate EMA from close prices by default', () => {
      const result = Effect.runSync(
        calculateEMAFromCandlesticks(sampleCandlesticks, 3),
      )
      // First value is SMA of first 3 closes: (105+110+115)/3 = 110
      // Multiplier = 2/(3+1) = 0.5
      // Second value: (120-110)*0.5 + 110 = 115
      // Third value: (125-115)*0.5 + 115 = 120
      expect(result[0]).toBeCloseTo(110)
      expect(result[1]).toBeCloseTo(115)
      expect(result[2]).toBeCloseTo(120)
    })

    it('should calculate EMA from specified price type', () => {
      const result = Effect.runSync(
        calculateEMAFromCandlesticks(sampleCandlesticks, 3, PriceType.Open),
      )
      // First value is SMA of first 3 opens: (100+105+110)/3 = 105
      // Multiplier = 2/(3+1) = 0.5
      // Second value: (115-105)*0.5 + 105 = 110
      // Third value: (120-110)*0.5 + 110 = 115
      expect(result[0]).toBeCloseTo(105)
      expect(result[1]).toBeCloseTo(110)
      expect(result[2]).toBeCloseTo(115)
    })

    it('should return empty array when not enough candlesticks', () => {
      const result = Effect.runSync(
        calculateEMAFromCandlesticks(sampleCandlesticks, 6),
      )
      expect(result).toEqual([])
    })
  })

  describe('createExponentialMovingAverage', () => {
    it('should create a valid EMA indicator', () => {
      const result = Effect.runSync(
        Effect.either(
          createExponentialMovingAverage({
            period: 3,
            priceType: PriceType.Close,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.name).toBe('Exponential Moving Average (3)')
        expect(result.right.description).toBe(
          'Exponentially weighted average price over 3 periods',
        )
        expect(result.right.parameters).toEqual({
          period: 3,
          priceType: PriceType.Close,
        })

        // Test the calculate function
        const calcResult = Effect.runSync(
          result.right.calculate(sampleCandlesticks),
        )
        expect(calcResult[0]).toBeCloseTo(110)
        expect(calcResult[1]).toBeCloseTo(115)
        expect(calcResult[2]).toBeCloseTo(120)
      }
    })

    it('should create a valid EMA indicator with default price type', () => {
      const result = Effect.runSync(
        Effect.either(
          createExponentialMovingAverage({
            period: 3,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.parameters).toEqual({
          period: 3,
          priceType: PriceType.Close,
        })
      }
    })

    it('should fail with non-positive period', () => {
      const result = Effect.runSync(
        Effect.either(
          createExponentialMovingAverage({
            period: 0,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidIndicatorParametersError)
        expect((result.left as InvalidIndicatorParametersError).message).toBe(
          'Period must be positive',
        )
      }
    })

    it('should handle not enough candlesticks gracefully', () => {
      const result = Effect.runSync(
        Effect.either(
          createExponentialMovingAverage({
            period: 10,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        // Test the calculate function with not enough candlesticks
        const calcResult = Effect.runSync(
          result.right.calculate(sampleCandlesticks),
        )
        expect(calcResult).toEqual([])
      }
    })
  })
})
