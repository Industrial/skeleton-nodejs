import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionSizingMethod } from './PositionSizingMethod'
import {
  type RunWalkForwardOptimizationInput,
  RunWalkForwardOptimizationInputSchema,
} from './RunWalkForwardOptimizationInput'

describe('RunWalkForwardOptimizationInput', () => {
  describe('RunWalkForwardOptimizationInputSchema', () => {
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
      name: 'Moving Average Crossover',
      description: 'Simple MA crossover strategy',
      parameters: {
        fastPeriod: 10,
        slowPeriod: 20,
      },
      indicators: [], // Required by StrategyBaseSchema
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

    it('should validate valid walk-forward optimization input with required fields', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [
          sampleParameters1,
          sampleParameters2,
          sampleParameters3,
        ],
        inSamplePercentage: 70,
        numFolds: 3,
      }

      const result = Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with optional fields', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [
          sampleParameters1,
          sampleParameters2,
          sampleParameters3,
        ],
        inSamplePercentage: 70,
        numFolds: 3,
        namePrefix: 'BTC/USDT MA Crossover WFO',
      }

      const result = Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with single parameter set', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1],
        inSamplePercentage: 70,
        numFolds: 3,
      }

      const result = Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with different in-sample percentage and folds', () => {
      const validInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 50, // Different value
        numFolds: 5, // Different value
      }

      const result = Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should reject input with empty parameters list', () => {
      const invalidInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [],
        inSamplePercentage: 70,
        numFolds: 3,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(invalidInput)
      }).toThrow()
    })

    it('should reject input with invalid in-sample percentage', () => {
      // Too low
      const tooLowPercentage = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 0, // Invalid: must be > 0
        numFolds: 3,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
          tooLowPercentage,
        )
      }).toThrow()

      // Too high
      const tooHighPercentage = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 100, // Invalid: must be < 100
        numFolds: 3,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
          tooHighPercentage,
        )
      }).toThrow()
    })

    it('should reject input with non-positive number of folds', () => {
      const invalidInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 70,
        numFolds: 0, // Invalid: must be positive
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(invalidInput)
      }).toThrow()

      const negativeInput = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 70,
        numFolds: -3, // Invalid: must be positive
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(negativeInput)
      }).toThrow()
    })

    it('should reject input with missing required fields', () => {
      // Missing strategy
      const missingStrategy = {
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 70,
        numFolds: 3,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
          missingStrategy as unknown as RunWalkForwardOptimizationInput,
        )
      }).toThrow()

      // Missing candlesticks
      const missingCandlesticks = {
        strategy: sampleStrategy,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 70,
        numFolds: 3,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
          missingCandlesticks as unknown as RunWalkForwardOptimizationInput,
        )
      }).toThrow()

      // Missing parametersList
      const missingParametersList = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        inSamplePercentage: 70,
        numFolds: 3,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
          missingParametersList as unknown as RunWalkForwardOptimizationInput,
        )
      }).toThrow()

      // Missing inSamplePercentage
      const missingInSamplePercentage = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        numFolds: 3,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
          missingInSamplePercentage as unknown as RunWalkForwardOptimizationInput,
        )
      }).toThrow()

      // Missing numFolds
      const missingNumFolds = {
        strategy: sampleStrategy,
        candlesticks: sampleCandlesticks,
        parametersList: [sampleParameters1, sampleParameters2],
        inSamplePercentage: 70,
      }

      expect(() => {
        Schema.decodeSync(RunWalkForwardOptimizationInputSchema)(
          missingNumFolds as unknown as RunWalkForwardOptimizationInput,
        )
      }).toThrow()
    })

    // Note: We don't test for invalid strategy, candlesticks, or parameters
    // because the schema relies on other schemas for validation
  })
})
