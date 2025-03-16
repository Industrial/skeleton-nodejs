import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PerformanceMetricsInputSchema } from './PerformanceMetricsInput'

describe('PerformanceMetricsInput', () => {
  describe('PerformanceMetricsInputSchema', () => {
    it('should validate valid performance metrics input with required fields', () => {
      const validInput = {
        totalReturn: 5000,
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        winRate: 60,
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: 2.4,
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000, // 1 day in milliseconds
      }

      const result = Schema.decodeSync(PerformanceMetricsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate performance metrics input with optional fields', () => {
      const validInput = {
        totalReturn: 5000,
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        winRate: 60,
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: 2.4,
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        sharpeRatio: 1.5,
        sortinoRatio: 2.1,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000,
        metadata: {
          strategy: 'MovingAverageCrossover',
          period: '2021-01-01 to 2021-12-31',
        },
      }

      const result = Schema.decodeSync(PerformanceMetricsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate performance metrics input with negative returns', () => {
      const negativeReturnInput = {
        totalReturn: -5000,
        totalReturnPercentage: -10,
        numberOfTrades: 20,
        winningTrades: 8,
        losingTrades: 12,
        winRate: 40,
        averageProfit: 600,
        averageLoss: 750,
        profitFactor: 0.8,
        maxDrawdown: 6000,
        maxDrawdownPercentage: 12,
        maxConsecutiveWins: 3,
        maxConsecutiveLosses: 5,
        averageHoldingPeriod: 86400000,
      }

      const result = Schema.decodeSync(PerformanceMetricsInputSchema)(
        negativeReturnInput,
      )
      expect(result).toEqual(negativeReturnInput)
    })

    it('should reject metrics input with win rate outside 0-100 range', () => {
      const invalidWinRateHigh = {
        totalReturn: 5000,
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        winRate: 101, // Invalid: > 100
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: 2.4,
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000,
      }

      expect(() => {
        Schema.decodeSync(PerformanceMetricsInputSchema)(invalidWinRateHigh)
      }).toThrow()

      const invalidWinRateLow = {
        totalReturn: 5000,
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        winRate: -1, // Invalid: < 0
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: 2.4,
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000,
      }

      expect(() => {
        Schema.decodeSync(PerformanceMetricsInputSchema)(invalidWinRateLow)
      }).toThrow()
    })

    it('should reject metrics input with negative profit factor', () => {
      const invalidProfitFactor = {
        totalReturn: 5000,
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        winRate: 60,
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: -2.4, // Invalid: < 0
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000,
      }

      expect(() => {
        Schema.decodeSync(PerformanceMetricsInputSchema)(invalidProfitFactor)
      }).toThrow()
    })

    it('should reject metrics input with missing required fields', () => {
      // Missing totalReturn
      const missingTotalReturn = {
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        winRate: 60,
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: 2.4,
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000,
      }

      expect(() => {
        Schema.decodeSync(PerformanceMetricsInputSchema)(
          missingTotalReturn as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing winRate
      const missingWinRate = {
        totalReturn: 5000,
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: 2.4,
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000,
      }

      expect(() => {
        Schema.decodeSync(PerformanceMetricsInputSchema)(
          missingWinRate as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject metrics input with invalid metadata', () => {
      const invalidMetadataInput = {
        totalReturn: 5000,
        totalReturnPercentage: 10,
        numberOfTrades: 20,
        winningTrades: 12,
        losingTrades: 8,
        winRate: 60,
        averageProfit: 600,
        averageLoss: 250,
        profitFactor: 2.4,
        maxDrawdown: 2000,
        maxDrawdownPercentage: 4,
        maxConsecutiveWins: 5,
        maxConsecutiveLosses: 3,
        averageHoldingPeriod: 86400000,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(PerformanceMetricsInputSchema)(
          invalidMetadataInput as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should validate metrics input with zero values for some fields', () => {
      const zeroValuesInput = {
        totalReturn: 0,
        totalReturnPercentage: 0,
        numberOfTrades: 0,
        winningTrades: 0,
        losingTrades: 0,
        winRate: 0,
        averageProfit: 0,
        averageLoss: 0,
        profitFactor: 0,
        maxDrawdown: 0,
        maxDrawdownPercentage: 0,
        maxConsecutiveWins: 0,
        maxConsecutiveLosses: 0,
        averageHoldingPeriod: 0,
      }

      const result = Schema.decodeSync(PerformanceMetricsInputSchema)(
        zeroValuesInput,
      )
      expect(result).toEqual(zeroValuesInput)
    })
  })
})
