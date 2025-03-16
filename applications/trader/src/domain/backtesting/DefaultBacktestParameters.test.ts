import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { DefaultBacktestParametersSchema } from './DefaultBacktestParameters'
import { PositionSizingMethod } from './PositionSizingMethod'

describe('DefaultBacktestParameters', () => {
  describe('DefaultBacktestParametersSchema', () => {
    it('should validate the default backtest parameters', () => {
      const defaultParams = {
        initialCapital: 10000,
        feeRate: 0.001, // 0.1%
        slippageRate: 0.001, // 0.1%
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10, // 10% of capital
        reinvestProfits: true,
        maxConcurrentPositions: 0, // unlimited
      }

      const result = Schema.decodeSync(DefaultBacktestParametersSchema)(
        defaultParams,
      )
      expect(result).toEqual(defaultParams)
    })

    it('should reject parameters with different values', () => {
      // Different initial capital
      const differentInitialCapital = {
        initialCapital: 20000, // Different from default 10000
        feeRate: 0.001,
        slippageRate: 0.001,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10,
        reinvestProfits: true,
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          differentInitialCapital,
        )
      }).toThrow()

      // Different fee rate
      const differentFeeRate = {
        initialCapital: 10000,
        feeRate: 0.002, // Different from default 0.001
        slippageRate: 0.001,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10,
        reinvestProfits: true,
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(differentFeeRate)
      }).toThrow()

      // Different slippage rate
      const differentSlippageRate = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.002, // Different from default 0.001
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10,
        reinvestProfits: true,
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          differentSlippageRate,
        )
      }).toThrow()

      // Different position sizing method
      const differentPositionSizingMethod = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.001,
        positionSizingMethod: PositionSizingMethod.Fixed, // Different from default PercentageOfCapital
        positionSizeValue: 10,
        reinvestProfits: true,
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          differentPositionSizingMethod,
        )
      }).toThrow()

      // Different position size value
      const differentPositionSizeValue = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.001,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 5, // Different from default 10
        reinvestProfits: true,
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          differentPositionSizeValue,
        )
      }).toThrow()

      // Different reinvest profits
      const differentReinvestProfits = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.001,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10,
        reinvestProfits: false, // Different from default true
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          differentReinvestProfits,
        )
      }).toThrow()

      // Different max concurrent positions
      const differentMaxConcurrentPositions = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.001,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10,
        reinvestProfits: true,
        maxConcurrentPositions: 5, // Different from default 0
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          differentMaxConcurrentPositions,
        )
      }).toThrow()
    })

    it('should reject parameters with missing fields', () => {
      // Missing initialCapital
      const missingInitialCapital = {
        feeRate: 0.001,
        slippageRate: 0.001,
        positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
        positionSizeValue: 10,
        reinvestProfits: true,
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          missingInitialCapital as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing positionSizingMethod
      const missingPositionSizingMethod = {
        initialCapital: 10000,
        feeRate: 0.001,
        slippageRate: 0.001,
        positionSizeValue: 10,
        reinvestProfits: true,
        maxConcurrentPositions: 0,
      }

      expect(() => {
        Schema.decodeSync(DefaultBacktestParametersSchema)(
          missingPositionSizingMethod as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
