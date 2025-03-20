import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'
import {
  type UpdateEquityCurveInput,
  UpdateEquityCurveInputSchema,
} from './UpdateEquityCurveInput'

describe('UpdateEquityCurveInput', () => {
  describe('UpdateEquityCurveInputSchema', () => {
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

    const sampleState = {
      equity: 10500,
      availableCapital: 9250,
      openPositions: [sampleOpenPosition],
      closedPositions: [],
      trades: [sampleTrade],
      equityCurve: [[1609459200000, 10000] as const],
      drawdownCurve: [[1609459200000, 0] as const],
      peakEquity: 10500,
    }

    it('should validate valid update equity curve input', () => {
      const validInput = {
        state: sampleState,
        timestamp: 1609545600000,
      }

      const result = Schema.decodeSync(UpdateEquityCurveInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with state having no open positions', () => {
      const stateWithNoOpenPositions = {
        ...sampleState,
        openPositions: [],
      }

      const validInput = {
        state: stateWithNoOpenPositions,
        timestamp: 1609545600000,
      }

      const result = Schema.decodeSync(UpdateEquityCurveInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with state having multiple open positions', () => {
      const stateWithMultipleOpenPositions = {
        ...sampleState,
        openPositions: [
          sampleOpenPosition,
          {
            ...sampleOpenPosition,
            id: 'position-456',
            entryPrice: 48000,
          },
        ],
      }

      const validInput = {
        state: stateWithMultipleOpenPositions,
        timestamp: 1609545600000,
      }

      const result = Schema.decodeSync(UpdateEquityCurveInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject input with non-positive timestamp', () => {
      const invalidInput = {
        state: sampleState,
        timestamp: 0, // Invalid: non-positive
      }

      expect(() => {
        Schema.decodeSync(UpdateEquityCurveInputSchema)(invalidInput)
      }).toThrow()

      const negativeTimestampInput = {
        state: sampleState,
        timestamp: -1609459200000, // Invalid: negative
      }

      expect(() => {
        Schema.decodeSync(UpdateEquityCurveInputSchema)(negativeTimestampInput)
      }).toThrow()
    })

    it('should reject input with missing required fields', () => {
      // Missing state
      const missingState = {
        timestamp: 1609545600000,
      }

      expect(() => {
        Schema.decodeSync(UpdateEquityCurveInputSchema)(
          missingState as unknown as UpdateEquityCurveInput,
        )
      }).toThrow()

      // Missing timestamp
      const missingTimestamp = {
        state: sampleState,
      }

      expect(() => {
        Schema.decodeSync(UpdateEquityCurveInputSchema)(
          missingTimestamp as unknown as UpdateEquityCurveInput,
        )
      }).toThrow()
    })

    // Note: We don't test for invalid state because the schema relies on
    // BacktestStateSchema for validation
  })
})
