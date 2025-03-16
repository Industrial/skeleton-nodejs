import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { BacktestParametersInputSchema } from './BacktestParametersInput'
import { PositionSizingMethod } from './PositionSizingMethod'

describe('BacktestParametersInput', () => {
  describe('BacktestParametersInputSchema', () => {
    it('should validate valid backtest parameters input with required fields', () => {
      const validParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      const result = Schema.decodeSync(BacktestParametersInputSchema)(
        validParams,
      )
      expect(result).toEqual(validParams)
    })

    it('should validate backtest parameters input with metadata', () => {
      const validParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10, // 10% of capital
        reinvestProfits: true,
        maxConcurrentPositions: 5,
        metadata: {
          description: 'Backtest for BTC/USDT with MA Crossover',
          market: 'crypto',
          exchange: 'binance',
        },
      }

      const result = Schema.decodeSync(BacktestParametersInputSchema)(
        validParams,
      )
      expect(result).toEqual(validParams)
    })

    it('should validate backtest parameters input with zero fees and slippage', () => {
      const validParams = {
        initialCapital: 10000,
        feeRate: 0,
        slippageRate: 0,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: false,
        maxConcurrentPositions: 0, // Unlimited
      }

      const result = Schema.decodeSync(BacktestParametersInputSchema)(
        validParams,
      )
      expect(result).toEqual(validParams)
    })

    it('should validate backtest parameters input with different position sizing methods', () => {
      // Fixed sizing
      const fixedSizingParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1, // 1 BTC
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      const fixedResult = Schema.decodeSync(BacktestParametersInputSchema)(
        fixedSizingParams,
      )
      expect(fixedResult).toEqual(fixedSizingParams)

      // Percentage of capital
      const percentageParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10, // 10% of capital
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      const percentageResult = Schema.decodeSync(BacktestParametersInputSchema)(
        percentageParams,
      )
      expect(percentageResult).toEqual(percentageParams)

      // Risk-based
      const riskBasedParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.RiskBased,
        positionSizeValue: 1, // 1% risk per trade
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      const riskBasedResult = Schema.decodeSync(BacktestParametersInputSchema)(
        riskBasedParams,
      )
      expect(riskBasedResult).toEqual(riskBasedParams)

      // Kelly criterion
      const kellyParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Kelly,
        positionSizeValue: 0.5, // Half-Kelly
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      const kellyResult = Schema.decodeSync(BacktestParametersInputSchema)(
        kellyParams,
      )
      expect(kellyResult).toEqual(kellyParams)
    })

    it('should reject parameters input with non-positive initial capital', () => {
      const invalidParams = {
        initialCapital: 0,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(invalidParams)
      }).toThrow()

      const negativeCapitalParams = {
        initialCapital: -10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(negativeCapitalParams)
      }).toThrow()
    })

    it('should reject parameters input with negative fee rate', () => {
      const invalidParams = {
        initialCapital: 10000,
        feeRate: -0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(invalidParams)
      }).toThrow()
    })

    it('should reject parameters input with negative slippage rate', () => {
      const invalidParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: -0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(invalidParams)
      }).toThrow()
    })

    it('should reject parameters input with invalid position sizing method', () => {
      const invalidParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: 'invalid' as PositionSizingMethod,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(invalidParams)
      }).toThrow()
    })

    it('should reject parameters input with non-positive position size value', () => {
      const invalidParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 0,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(invalidParams)
      }).toThrow()

      const negativePositionSizeParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: -1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(
          negativePositionSizeParams,
        )
      }).toThrow()
    })

    it('should reject parameters input with negative max concurrent positions', () => {
      const invalidParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: -1,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(invalidParams)
      }).toThrow()
    })

    it('should reject parameters input with missing required fields', () => {
      // Missing initialCapital
      const missingInitialCapital = {
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(
          missingInitialCapital as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing positionSizingMethod
      const missingPositionSizingMethod = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(
          missingPositionSizingMethod as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject parameters input with invalid metadata', () => {
      const invalidMetadataParams = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.0005,
        positionSizingMethod: PositionSizingMethod.Fixed,
        positionSizeValue: 1,
        reinvestProfits: true,
        maxConcurrentPositions: 5,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(BacktestParametersInputSchema)(
          invalidMetadataParams as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
