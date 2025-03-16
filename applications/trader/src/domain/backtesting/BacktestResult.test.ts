import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { BacktestResultSchema } from './BacktestResult'
import { PositionSizingMethod } from './PositionSizingMethod'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'

describe('BacktestResult', () => {
  describe('BacktestResultSchema', () => {
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

    const sampleTrade = {
      direction: TradeDirection.Buy,
      price: 50000,
      volume: 1.5,
      timestamp: 1609459200000,
      fees: 0.001,
    }

    const samplePosition = {
      id: 'position-123',
      status: PositionStatus.Closed,
      direction: TradeDirection.Buy,
      entryPrice: 50000,
      size: 1.5,
      openTimestamp: 1609459200000,
      exitPrice: 55000,
      closeTimestamp: 1609545600000,
      entryTrade: sampleTrade,
      exitTrade: {
        direction: TradeDirection.Sell,
        price: 55000,
        volume: 1.5,
        timestamp: 1609545600000,
        fees: 0.001,
      },
    }

    const sampleMetrics = {
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

    it('should validate a valid backtest result with required fields', () => {
      const validResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
      }

      const result = Schema.decodeSync(BacktestResultSchema)(validResult)
      expect(result).toEqual(validResult)
    })

    it('should validate a backtest result with optional fields', () => {
      const validResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        description:
          'Testing MA Crossover strategy on BTC/USDT with 10/20 periods',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
        metadata: {
          exchange: 'binance',
          timeframe: '1h',
          strategyParameters: {
            fastPeriod: 10,
            slowPeriod: 20,
          },
        },
      }

      const result = Schema.decodeSync(BacktestResultSchema)(validResult)
      expect(result).toEqual(validResult)
    })

    it('should validate a backtest result with empty trades and positions', () => {
      const emptyResult = {
        id: 'backtest-123',
        name: 'Empty Backtest',
        parameters: sampleParameters,
        trades: [],
        positions: [],
        metrics: {
          ...sampleMetrics,
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
        },
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 500,
      }

      const result = Schema.decodeSync(BacktestResultSchema)(emptyResult)
      expect(result).toEqual(emptyResult)
    })

    it('should reject a result with non-positive start timestamp', () => {
      const invalidResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 0, // Invalid: non-positive
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(invalidResult)
      }).toThrow()

      const negativeStartTimestampResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: -1609459200000, // Invalid: negative
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(negativeStartTimestampResult)
      }).toThrow()
    })

    it('should reject a result with non-positive end timestamp', () => {
      const invalidResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 0, // Invalid: non-positive
        runTimestamp: 1609632000000,
        executionTime: 1500,
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(invalidResult)
      }).toThrow()
    })

    it('should reject a result with non-positive run timestamp', () => {
      const invalidResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 0, // Invalid: non-positive
        executionTime: 1500,
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(invalidResult)
      }).toThrow()
    })

    it('should reject a result with negative execution time', () => {
      const invalidResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: -1500, // Invalid: negative
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(invalidResult)
      }).toThrow()
    })

    it('should reject a result with missing required fields', () => {
      // Missing id
      const missingId = {
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(
          missingId as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing name
      const missingName = {
        id: 'backtest-123',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(
          missingName as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing parameters
      const missingParameters = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(
          missingParameters as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a result with invalid metadata', () => {
      const invalidMetadataResult = {
        id: 'backtest-123',
        name: 'BTC/USDT MA Crossover Backtest',
        parameters: sampleParameters,
        trades: [sampleTrade, samplePosition.exitTrade],
        positions: [samplePosition],
        metrics: sampleMetrics,
        startTimestamp: 1609459200000,
        endTimestamp: 1609545600000,
        runTimestamp: 1609632000000,
        executionTime: 1500,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(BacktestResultSchema)(
          invalidMetadataResult as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
