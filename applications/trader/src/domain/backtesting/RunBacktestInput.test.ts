import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionSizingMethod } from './PositionSizingMethod'
import { RunBacktestInputSchema } from './RunBacktestInput'

describe('RunBacktestInput', () => {
  describe('RunBacktestInputSchema', () => {
    // Sample data for testing
    const sampleParameters = {
      initialCapital: 10000,
      feeRate: 0.001,
      slippageRate: 0.0005,
      positionSizingMethod: PositionSizingMethod.Fixed,
      positionSizeValue: 1,
      reinvestProfits: true,
      maxConcurrentPositions: 5,
    }

    const sampleStrategy = {
      id: 'ma-crossover',
      name: 'Moving Average Crossover',
      description: 'Simple MA crossover strategy',
      version: '1.0.0',
      parameters: {
        fastPeriod: 10,
        slowPeriod: 20,
      },
      generateSignals: () => [], // Mock function
    }

    const sampleCandlesticks = [
      {
        timestamp: 1609459200000,
        open: 49000,
        high: 51000,
        low: 48500,
        close: 50000,
        volume: 100,
      },
      {
        timestamp: 1609545600000,
        open: 54000,
        high: 56000,
        low: 53500,
        close: 55000,
        volume: 120,
      },
    ]

    it('should validate valid run backtest input with required fields', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
      }

      const result = Schema.decodeSync(RunBacktestInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate run backtest input with optional fields', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
        name: 'BTC/USDT MA Crossover Backtest',
        description:
          'Testing MA Crossover strategy on BTC/USDT with 10/20 periods',
      }

      const result = Schema.decodeSync(RunBacktestInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with empty candlesticks array', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: [],
        parameters: sampleParameters,
      }

      const result = Schema.decodeSync(RunBacktestInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with different strategy', () => {
      const differentStrategy = {
        id: 'rsi-strategy',
        name: 'RSI Strategy',
        description: 'Strategy based on RSI indicator',
        version: '1.0.0',
        parameters: {
          period: 14,
          overbought: 70,
          oversold: 30,
        },
        generateSignals: () => [], // Mock function
      }

      const validInput = {
        strategy: differentStrategy,
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
      }

      const result = Schema.decodeSync(RunBacktestInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject input with missing required fields', () => {
      // Missing strategy
      const missingStrategy = {
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
      }

      expect(() => {
        Schema.decodeSync(RunBacktestInputSchema)(
          missingStrategy as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing candlesticks
      const missingCandlesticks = {
        strategy: sampleStrategy,
        parameters: sampleParameters,
      }

      expect(() => {
        Schema.decodeSync(RunBacktestInputSchema)(
          missingCandlesticks as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing parameters
      const missingParameters = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
      }

      expect(() => {
        Schema.decodeSync(RunBacktestInputSchema)(
          missingParameters as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    // Note: We don't test for invalid strategy, candlesticks, or parameters
    // because the schema relies on StrategyBaseSchema and BacktestParametersSchema for validation
  })
})
