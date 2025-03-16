import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import { PriceType } from './Indicator'
import {
  InvalidStrategyParametersError,
  createBaseStrategyParameters,
  createMovingAverageCrossoverParameters,
  createRSIStrategyParameters,
} from './StrategyParameters'

describe('StrategyParameters', () => {
  describe('createBaseStrategyParameters', () => {
    it('should create valid base strategy parameters', () => {
      const result = Effect.runSync(
        Effect.either(
          createBaseStrategyParameters({
            name: 'Test Strategy',
            description: 'A test strategy',
            metadata: { author: 'Test Author', version: '1.0.0' },
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          name: 'Test Strategy',
          description: 'A test strategy',
          metadata: { author: 'Test Author', version: '1.0.0' },
        })
      }
    })

    it('should create valid base strategy parameters without optional fields', () => {
      const result = Effect.runSync(
        Effect.either(
          createBaseStrategyParameters({
            name: 'Test Strategy',
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          name: 'Test Strategy',
        })
      }
    })

    it('should fail with empty name', () => {
      const result = Effect.runSync(
        Effect.either(
          createBaseStrategyParameters({
            name: '',
            description: 'A test strategy',
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Strategy name cannot be empty',
        )
      }
    })
  })

  describe('createMovingAverageCrossoverParameters', () => {
    it('should create valid moving average crossover parameters', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverParameters({
            name: 'MA Crossover',
            description: 'Simple moving average crossover strategy',
            fastPeriod: 10,
            slowPeriod: 20,
            priceType: PriceType.Close,
            metadata: { author: 'Test Author', version: '1.0.0' },
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          name: 'MA Crossover',
          description: 'Simple moving average crossover strategy',
          fastPeriod: 10,
          slowPeriod: 20,
          priceType: PriceType.Close,
          metadata: { author: 'Test Author', version: '1.0.0' },
        })
      }
    })

    it('should create valid moving average crossover parameters without optional fields', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverParameters({
            name: 'MA Crossover',
            fastPeriod: 10,
            slowPeriod: 20,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          name: 'MA Crossover',
          fastPeriod: 10,
          slowPeriod: 20,
        })
      }
    })

    it('should fail with empty name', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverParameters({
            name: '',
            fastPeriod: 10,
            slowPeriod: 20,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Strategy name cannot be empty',
        )
      }
    })

    it('should fail with non-positive fast period', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverParameters({
            name: 'MA Crossover',
            fastPeriod: 0,
            slowPeriod: 20,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Fast period must be positive',
        )
      }
    })

    it('should fail with non-positive slow period', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverParameters({
            name: 'MA Crossover',
            fastPeriod: 10,
            slowPeriod: 0,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Slow period must be positive',
        )
      }
    })

    it('should fail when fast period is greater than or equal to slow period', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverParameters({
            name: 'MA Crossover',
            fastPeriod: 20,
            slowPeriod: 20,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Fast period must be less than slow period',
        )
      }
    })
  })

  describe('createRSIStrategyParameters', () => {
    it('should create valid RSI strategy parameters', () => {
      const result = Effect.runSync(
        Effect.either(
          createRSIStrategyParameters({
            name: 'RSI Strategy',
            description: 'Relative Strength Index strategy',
            period: 14,
            overbought: 70,
            oversold: 30,
            priceType: PriceType.Close,
            metadata: { author: 'Test Author', version: '1.0.0' },
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          name: 'RSI Strategy',
          description: 'Relative Strength Index strategy',
          period: 14,
          overbought: 70,
          oversold: 30,
          priceType: PriceType.Close,
          metadata: { author: 'Test Author', version: '1.0.0' },
        })
      }
    })

    it('should create valid RSI strategy parameters without optional fields', () => {
      const result = Effect.runSync(
        Effect.either(
          createRSIStrategyParameters({
            name: 'RSI Strategy',
            period: 14,
            overbought: 70,
            oversold: 30,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          name: 'RSI Strategy',
          period: 14,
          overbought: 70,
          oversold: 30,
        })
      }
    })

    it('should fail with empty name', () => {
      const result = Effect.runSync(
        Effect.either(
          createRSIStrategyParameters({
            name: '',
            period: 14,
            overbought: 70,
            oversold: 30,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Strategy name cannot be empty',
        )
      }
    })

    it('should fail with non-positive period', () => {
      const result = Effect.runSync(
        Effect.either(
          createRSIStrategyParameters({
            name: 'RSI Strategy',
            period: 0,
            overbought: 70,
            oversold: 30,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Period must be positive',
        )
      }
    })

    it('should fail when overbought is less than or equal to oversold', () => {
      const result = Effect.runSync(
        Effect.either(
          createRSIStrategyParameters({
            name: 'RSI Strategy',
            period: 14,
            overbought: 30,
            oversold: 30,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Overbought threshold must be greater than oversold threshold',
        )
      }
    })

    it('should fail when overbought is outside the valid range', () => {
      const result = Effect.runSync(
        Effect.either(
          createRSIStrategyParameters({
            name: 'RSI Strategy',
            period: 14,
            overbought: 110,
            oversold: 30,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Overbought threshold must be between 0 and 100',
        )
      }
    })

    it('should fail when oversold is outside the valid range', () => {
      const result = Effect.runSync(
        Effect.either(
          createRSIStrategyParameters({
            name: 'RSI Strategy',
            period: 14,
            overbought: 70,
            oversold: -10,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Oversold threshold must be between 0 and 100',
        )
      }
    })
  })
})
