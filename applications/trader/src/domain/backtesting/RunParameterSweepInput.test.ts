import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionSizingMethod } from './PositionSizingMethod'
import { RunParameterSweepInputSchema } from './RunParameterSweepInput'

describe('RunParameterSweepInput', () => {
  describe('RunParameterSweepInputSchema', () => {
    // Sample data for testing
    const sampleParameters1 = {
      initialCapital: 10000,
      feeRate: 0.001,
      slippageRate: 0.0005,
      positionSizingMethod: PositionSizingMethod.Fixed,
      positionSizeValue: 1,
      reinvestProfits: true,
      maxConcurrentPositions: 5,
    }

    const sampleParameters2 = {
      initialCapital: 10000,
      feeRate: 0.001,
      slippageRate: 0.0005,
      positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
      positionSizeValue: 10,
      reinvestProfits: true,
      maxConcurrentPositions: 5,
    }

    const sampleParameters3 = {
      initialCapital: 10000,
      feeRate: 0.001,
      slippageRate: 0.0005,
      positionSizingMethod: PositionSizingMethod.RiskBased,
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

    it('should validate valid run parameter sweep input with required fields', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [
          sampleParameters1,
          sampleParameters2,
          sampleParameters3,
        ],
      }

      const result = Schema.decodeSync(RunParameterSweepInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate run parameter sweep input with optional fields', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [
          sampleParameters1,
          sampleParameters2,
          sampleParameters3,
        ],
        namePrefix: 'BTC/USDT MA Crossover Sweep',
      }

      const result = Schema.decodeSync(RunParameterSweepInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with single parameter set', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1],
      }

      const result = Schema.decodeSync(RunParameterSweepInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with empty candlesticks array', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: [],
        parametersList: [sampleParameters1, sampleParameters2],
      }

      const result = Schema.decodeSync(RunParameterSweepInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject input with empty parameters list', () => {
      const invalidInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [],
      }

      expect(() => {
        Schema.decodeSync(RunParameterSweepInputSchema)(invalidInput)
      }).toThrow()
    })

    it('should reject input with missing required fields', () => {
      // Missing strategy
      const missingStrategy = {
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
      }

      expect(() => {
        Schema.decodeSync(RunParameterSweepInputSchema)(
          missingStrategy as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing candlesticks
      const missingCandlesticks = {
        strategy: sampleStrategy,
        parametersList: [sampleParameters1, sampleParameters2],
      }

      expect(() => {
        Schema.decodeSync(RunParameterSweepInputSchema)(
          missingCandlesticks as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing parametersList
      const missingParametersList = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
      }

      expect(() => {
        Schema.decodeSync(RunParameterSweepInputSchema)(
          missingParametersList as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    // Note: We don't test for invalid strategy, candlesticks, or parameters
    // because the schema relies on other schemas for validation
  })
})
