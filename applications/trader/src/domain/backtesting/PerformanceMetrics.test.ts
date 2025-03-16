import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PerformanceMetricsSchema } from './PerformanceMetrics'

describe('PerformanceMetrics', () => {
  describe('PerformanceMetricsSchema', () => {
    it('should validate valid performance metrics with required fields', () => {
      const validMetrics = {
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

      const result = Schema.decodeSync(PerformanceMetricsSchema)(validMetrics)
      expect(result).toEqual(validMetrics)
    })

    it('should validate performance metrics with optional fields', () => {
      const validMetrics = {
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

      const result = Schema.decodeSync(PerformanceMetricsSchema)(validMetrics)
      expect(result).toEqual(validMetrics)
    })

    it('should validate performance metrics with negative returns', () => {
      const negativeReturnMetrics = {
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

      const result = Schema.decodeSync(PerformanceMetricsSchema)(
        negativeReturnMetrics,
      )
      expect(result).toEqual(negativeReturnMetrics)
    })

    it('should reject metrics with win rate outside 0-100 range', () => {
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
        Schema.decodeSync(PerformanceMetricsSchema)(invalidWinRateHigh)
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
        Schema.decodeSync(PerformanceMetricsSchema)(invalidWinRateLow)
      }).toThrow()
    })

    it('should reject metrics with negative profit factor', () => {
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
        Schema.decodeSync(PerformanceMetricsSchema)(invalidProfitFactor)
      }).toThrow()
    })

    it('should reject metrics with missing required fields', () => {
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
        Schema.decodeSync(PerformanceMetricsSchema)(
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
        Schema.decodeSync(PerformanceMetricsSchema)(
          missingWinRate as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject metrics with invalid metadata', () => {
      const invalidMetadataMetrics = {
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
        Schema.decodeSync(PerformanceMetricsSchema)(
          invalidMetadataMetrics as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should validate metrics with zero values for some fields', () => {
      const zeroValuesMetrics = {
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

      const result = Schema.decodeSync(PerformanceMetricsSchema)(
        zeroValuesMetrics,
      )
      expect(result).toEqual(zeroValuesMetrics)
    })
  })
})
