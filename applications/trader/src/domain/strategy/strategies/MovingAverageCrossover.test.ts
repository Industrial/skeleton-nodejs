import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import type { Candlestick } from '../../market-data/Candlestick'
import { PriceType } from '../Indicator'
import { SignalDirection } from '../Signal'
import { InvalidStrategyParametersError } from '../StrategyParameters'
import {
  MovingAverageType,
  createMovingAverageCrossoverStrategy,
} from './MovingAverageCrossover'

describe('MovingAverageCrossover', () => {
  // Sample candlesticks for testing
  // These values are designed to create specific crossover scenarios
  const sampleCandlesticks: Candlestick[] = [
    // First few candlesticks to establish initial MA values
    {
      timestamp: 1609459200000, // 2021-01-01
      open: 100,
      high: 110,
      low: 90,
      close: 100, // Fast MA will start higher than slow MA
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
      close: 105, // Fast MA starts to decline
      volume: 1500,
    },
    // Crossover point: Fast MA crosses below Slow MA (sell signal)
    {
      timestamp: 1609718400000, // 2021-01-04
      open: 100,
      high: 110,
      low: 95,
      close: 95, // Fast MA continues to decline
      volume: 1800,
    },
    {
      timestamp: 1609804800000, // 2021-01-05
      open: 90,
      high: 100,
      low: 85,
      close: 90, // Fast MA now below Slow MA
      volume: 2000,
    },
    // Fast MA starts to rise again
    {
      timestamp: 1609891200000, // 2021-01-06
      open: 95,
      high: 105,
      low: 90,
      close: 100,
      volume: 1800,
    },
    // Crossover point: Fast MA crosses above Slow MA (buy signal)
    {
      timestamp: 1609977600000, // 2021-01-07
      open: 105,
      high: 115,
      low: 100,
      close: 110, // Fast MA now above Slow MA
      volume: 1600,
    },
    {
      timestamp: 1610064000000, // 2021-01-08
      open: 110,
      high: 120,
      low: 105,
      close: 115, // Fast MA continues to rise
      volume: 1400,
    },
  ]

  describe('createMovingAverageCrossoverStrategy', () => {
    it('should create a valid strategy with simple moving averages', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverStrategy({
            name: 'MA Crossover',
            description: 'Simple moving average crossover strategy',
            fastPeriod: 2,
            slowPeriod: 4,
            priceType: PriceType.Close,
            maType: MovingAverageType.Simple,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.name).toBe('MA Crossover')
        expect(result.right.description).toBe(
          'Simple moving average crossover strategy',
        )
        expect(result.right.parameters).toEqual({
          fastPeriod: 2,
          slowPeriod: 4,
          priceType: PriceType.Close,
          maType: MovingAverageType.Simple,
        })
        expect(result.right.indicators.length).toBe(2)
      }
    })

    it('should create a valid strategy with exponential moving averages', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverStrategy({
            name: 'EMA Crossover',
            description: 'Exponential moving average crossover strategy',
            fastPeriod: 2,
            slowPeriod: 4,
            priceType: PriceType.Close,
            maType: MovingAverageType.Exponential,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.parameters.maType).toBe(
          MovingAverageType.Exponential,
        )
      }
    })

    it('should create a valid strategy with default parameters', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverStrategy({
            name: 'Default MA Crossover',
            fastPeriod: 2,
            slowPeriod: 4,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right.parameters).toEqual({
          fastPeriod: 2,
          slowPeriod: 4,
          priceType: PriceType.Close,
          maType: MovingAverageType.Simple,
        })
      }
    })

    it('should fail with non-positive fast period', () => {
      const result = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverStrategy({
            name: 'Invalid Strategy',
            fastPeriod: 0,
            slowPeriod: 4,
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
          createMovingAverageCrossoverStrategy({
            name: 'Invalid Strategy',
            fastPeriod: 2,
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
          createMovingAverageCrossoverStrategy({
            name: 'Invalid Strategy',
            fastPeriod: 4,
            slowPeriod: 4,
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

  describe('Strategy analyze function', () => {
    it('should generate correct signals for simple moving average crossovers', () => {
      const strategyResult = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverStrategy({
            name: 'Test MA Crossover',
            fastPeriod: 2,
            slowPeriod: 4,
            maType: MovingAverageType.Simple,
          }),
        ),
      )

      expect(Either.isRight(strategyResult)).toBe(true)
      if (Either.isRight(strategyResult)) {
        const strategy = strategyResult.right

        // Analyze the candlesticks
        const signalsResult = Effect.runSync(
          Effect.either(strategy.analyze(sampleCandlesticks)),
        )

        expect(Either.isRight(signalsResult)).toBe(true)
        if (Either.isRight(signalsResult)) {
          const signals = signalsResult.right

          // We should have signals starting from the second value of the slower MA
          // which is at index 3 (since slowPeriod is 4)
          expect(signals.length).toBeGreaterThan(0)

          // Check for the sell signal (fast MA crosses below slow MA)
          const sellSignalIndex = signals.findIndex(
            (s) => s.direction === SignalDirection.Sell,
          )
          expect(sellSignalIndex).not.toBe(-1)
          if (sellSignalIndex !== -1) {
            const sellSignal = signals[sellSignalIndex]
            expect(sellSignal.metadata?.crossover).toBe('below')
          }

          // Check for the buy signal (fast MA crosses above slow MA)
          const buySignalIndex = signals.findIndex(
            (s) => s.direction === SignalDirection.Buy,
          )
          expect(buySignalIndex).not.toBe(-1)
          if (buySignalIndex !== -1) {
            const buySignal = signals[buySignalIndex]
            expect(buySignal.metadata?.crossover).toBe('above')
          }

          // Verify the order: sell signal should come before buy signal
          if (sellSignalIndex !== -1 && buySignalIndex !== -1) {
            expect(sellSignalIndex).toBeLessThan(buySignalIndex)
          }
        }
      }
    })

    it('should generate correct signals for exponential moving average crossovers', () => {
      const strategyResult = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverStrategy({
            name: 'Test EMA Crossover',
            fastPeriod: 2,
            slowPeriod: 4,
            maType: MovingAverageType.Exponential,
          }),
        ),
      )

      expect(Either.isRight(strategyResult)).toBe(true)
      if (Either.isRight(strategyResult)) {
        const strategy = strategyResult.right

        // Analyze the candlesticks
        const signalsResult = Effect.runSync(
          Effect.either(strategy.analyze(sampleCandlesticks)),
        )

        expect(Either.isRight(signalsResult)).toBe(true)
        if (Either.isRight(signalsResult)) {
          const signals = signalsResult.right

          // We should have signals starting from the second value of the slower MA
          expect(signals.length).toBeGreaterThan(0)

          // Check that we have both buy and sell signals
          const hasSellSignal = signals.some(
            (s) => s.direction === SignalDirection.Sell,
          )
          const hasBuySignal = signals.some(
            (s) => s.direction === SignalDirection.Buy,
          )

          expect(hasSellSignal).toBe(true)
          expect(hasBuySignal).toBe(true)
        }
      }
    })

    it('should handle insufficient data gracefully', () => {
      const strategyResult = Effect.runSync(
        Effect.either(
          createMovingAverageCrossoverStrategy({
            name: 'Test MA Crossover',
            fastPeriod: 10, // Longer than our sample data
            slowPeriod: 20,
          }),
        ),
      )

      expect(Either.isRight(strategyResult)).toBe(true)
      if (Either.isRight(strategyResult)) {
        const strategy = strategyResult.right

        // Analyze the candlesticks
        const signalsResult = Effect.runSync(
          Effect.either(strategy.analyze(sampleCandlesticks)),
        )

        expect(Either.isRight(signalsResult)).toBe(true)
        if (Either.isRight(signalsResult)) {
          const signals = signalsResult.right
          // Not enough data to generate signals
          expect(signals.length).toBe(0)
        }
      }
    })
  })
})
