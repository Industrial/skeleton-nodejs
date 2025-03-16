import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import type { Candlestick } from '../market-data/Candlestick'
import { PriceType } from './Indicator'
import { SignalDirection, createBuySignal } from './Signal'
import { Strategy, StrategyExecutionError, createStrategy } from './Strategy'
import { InvalidStrategyParametersError } from './StrategyParameters'

describe('Strategy', () => {
  // Sample indicator for testing
  const sampleIndicator = {
    name: 'Test Indicator',
    description: 'A test indicator',
    parameters: { period: 14 },
    calculate: (candlesticks: Candlestick[]) =>
      Effect.succeed(candlesticks.length),
  }

  // Sample candlestick for testing
  const sampleCandlestick: Candlestick = {
    timestamp: 1609459200000, // 2021-01-01
    open: 100,
    high: 110,
    low: 90,
    close: 105,
    volume: 1000,
  }

  // Sample analyze function for testing
  const sampleAnalyzeFn = (candlesticks: Candlestick[]) =>
    Effect.succeed([
      {
        direction: SignalDirection.Buy,
        price: candlesticks[0].close,
        timestamp: candlesticks[0].timestamp,
      },
    ])

  describe('createStrategy', () => {
    it('should create a valid strategy', () => {
      const result = Effect.runSync(
        Effect.either(
          createStrategy(
            'Test Strategy',
            'A test strategy',
            { param1: 'value1', param2: 42 },
            [sampleIndicator],
            sampleAnalyzeFn,
          ),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.name).toBe('Test Strategy')
        expect(result.right.description).toBe('A test strategy')
        expect(result.right.parameters).toEqual({
          param1: 'value1',
          param2: 42,
        })
        expect(result.right.indicators).toEqual([sampleIndicator])

        // Test the analyze function
        const analyzeResult = Effect.runSync(
          result.right.analyze([sampleCandlestick]),
        )
        expect(analyzeResult).toEqual([
          {
            direction: SignalDirection.Buy,
            price: 105,
            timestamp: 1609459200000,
          },
        ])
      }
    })

    it('should fail with empty name', () => {
      const result = Effect.runSync(
        Effect.either(
          createStrategy(
            '',
            'A test strategy',
            { param1: 'value1' },
            [sampleIndicator],
            sampleAnalyzeFn,
          ),
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

    it('should fail with empty description', () => {
      const result = Effect.runSync(
        Effect.either(
          createStrategy(
            'Test Strategy',
            '',
            { param1: 'value1' },
            [sampleIndicator],
            sampleAnalyzeFn,
          ),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidStrategyParametersError)
        expect((result.left as InvalidStrategyParametersError).message).toBe(
          'Strategy description cannot be empty',
        )
      }
    })
  })

  describe('Strategy analyze function', () => {
    it('should handle errors in the analyze function', () => {
      // Create a strategy with an analyze function that fails
      const errorAnalyzeFn = () =>
        Effect.fail(
          new StrategyExecutionError({
            message: 'Analysis failed',
          }),
        )

      const strategyResult = Effect.runSync(
        Effect.either(
          createStrategy(
            'Error Strategy',
            'A strategy that fails',
            {},
            [],
            errorAnalyzeFn,
          ),
        ),
      )

      expect(Either.isRight(strategyResult)).toBe(true)
      if (Either.isRight(strategyResult)) {
        const strategy = strategyResult.right

        // Test that the analyze function properly propagates errors
        const analyzeResult = Effect.runSync(
          Effect.either(strategy.analyze([sampleCandlestick])),
        )

        expect(Either.isLeft(analyzeResult)).toBe(true)
        if (Either.isLeft(analyzeResult)) {
          expect(analyzeResult.left).toBeInstanceOf(StrategyExecutionError)
          expect((analyzeResult.left as StrategyExecutionError).message).toBe(
            'Analysis failed',
          )
        }
      }
    })
  })
})
