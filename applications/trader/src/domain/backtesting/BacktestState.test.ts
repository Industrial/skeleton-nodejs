import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { BacktestStateSchema } from './BacktestState'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'

describe('BacktestState', () => {
  describe('BacktestStateSchema', () => {
    // Sample data for testing
    const sampleTrade = {
      direction: TradeDirection.Buy,
      price: 50000,
      volume: 1.5,
      timestamp: 1609459200000,
      fees: 0.001,
    }

    const sampleOpenPosition = {
      id: 'position-123',
      status: PositionStatus.Open,
      direction: TradeDirection.Buy,
      entryPrice: 50000,
      size: 1.5,
      openTimestamp: 1609459200000,
      entryTrade: sampleTrade,
    }

    const sampleClosedPosition = {
      id: 'position-456',
      status: PositionStatus.Closed,
      direction: TradeDirection.Buy,
      entryPrice: 48000,
      size: 1.0,
      openTimestamp: 1609372800000, // 1 day earlier
      exitPrice: 52000,
      closeTimestamp: 1609545600000, // 1 day later
      entryTrade: {
        ...sampleTrade,
        price: 48000,
        volume: 1.0,
        timestamp: 1609372800000,
      },
      exitTrade: {
        direction: TradeDirection.Sell,
        price: 52000,
        volume: 1.0,
        timestamp: 1609545600000,
        fees: 0.001,
      },
    }

    it('should validate a valid backtest state with required fields', () => {
      const validState = {
        equity: 10500,
        availableCapital: 9250,
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 10250],
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: 10500,
      }

      const result = Schema.decodeSync(BacktestStateSchema)(validState)
      expect(result).toEqual(validState)
    })

    it('should validate a backtest state with metadata', () => {
      const validState = {
        equity: 10500,
        availableCapital: 9250,
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 10250],
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: 10500,
        metadata: {
          currentTimestamp: 1609545600000,
          status: 'running',
          progress: 0.5,
        },
      }

      const result = Schema.decodeSync(BacktestStateSchema)(validState)
      expect(result).toEqual(validState)
    })

    it('should validate a backtest state with empty arrays', () => {
      const emptyState = {
        equity: 10000,
        availableCapital: 10000,
        openPositions: [],
        closedPositions: [],
        trades: [],
        equityCurve: [[1609372800000, 10000]],
        drawdownCurve: [[1609372800000, 0]],
        peakEquity: 10000,
      }

      const result = Schema.decodeSync(BacktestStateSchema)(emptyState)
      expect(result).toEqual(emptyState)
    })

    it('should validate a backtest state with drawdown', () => {
      const stateWithDrawdown = {
        equity: 9000,
        availableCapital: 9000,
        openPositions: [],
        closedPositions: [
          {
            ...sampleClosedPosition,
            exitPrice: 44000, // Loss
          },
        ],
        trades: [
          sampleTrade,
          {
            ...sampleClosedPosition.exitTrade,
            price: 44000,
          },
        ],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 9500],
          [1609545600000, 9000],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 5],
          [1609545600000, 10],
        ],
        peakEquity: 10000,
      }

      const result = Schema.decodeSync(BacktestStateSchema)(stateWithDrawdown)
      expect(result).toEqual(stateWithDrawdown)
    })

    it('should reject a state with negative equity', () => {
      const invalidState = {
        equity: -500,
        availableCapital: 0,
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 5000],
          [1609545600000, -500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 50],
          [1609545600000, 105],
        ],
        peakEquity: 10000,
      }

      expect(() => {
        Schema.decodeSync(BacktestStateSchema)(invalidState)
      }).toThrow()
    })

    it('should reject a state with negative available capital', () => {
      const invalidState = {
        equity: 10500,
        availableCapital: -250, // Invalid: negative
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 10250],
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: 10500,
      }

      expect(() => {
        Schema.decodeSync(BacktestStateSchema)(invalidState)
      }).toThrow()
    })

    it('should reject a state with negative peak equity', () => {
      const invalidState = {
        equity: 10500,
        availableCapital: 9250,
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 10250],
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: -10500, // Invalid: negative
      }

      expect(() => {
        Schema.decodeSync(BacktestStateSchema)(invalidState)
      }).toThrow()
    })

    it('should reject a state with missing required fields', () => {
      // Missing equity
      const missingEquity = {
        availableCapital: 9250,
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 10250],
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: 10500,
      }

      expect(() => {
        Schema.decodeSync(BacktestStateSchema)(
          missingEquity as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing openPositions
      const missingOpenPositions = {
        equity: 10500,
        availableCapital: 9250,
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 10250],
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: 10500,
      }

      expect(() => {
        Schema.decodeSync(BacktestStateSchema)(
          missingOpenPositions as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a state with invalid metadata', () => {
      const invalidMetadataState = {
        equity: 10500,
        availableCapital: 9250,
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [1609459200000, 10250],
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: 10500,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(BacktestStateSchema)(
          invalidMetadataState as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a state with invalid equity curve format', () => {
      const invalidEquityCurveState = {
        equity: 10500,
        availableCapital: 9250,
        openPositions: [sampleOpenPosition],
        closedPositions: [sampleClosedPosition],
        trades: [sampleTrade, sampleClosedPosition.exitTrade],
        equityCurve: [
          [1609372800000, 10000],
          [10250], // Invalid: missing timestamp
          [1609545600000, 10500],
        ],
        drawdownCurve: [
          [1609372800000, 0],
          [1609459200000, 0],
          [1609545600000, 0],
        ],
        peakEquity: 10500,
      }

      expect(() => {
        Schema.decodeSync(BacktestStateSchema)(
          invalidEquityCurveState as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
