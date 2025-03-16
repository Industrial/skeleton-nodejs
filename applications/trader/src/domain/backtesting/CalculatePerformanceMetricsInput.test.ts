import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { CalculatePerformanceMetricsInputSchema } from './CalculatePerformanceMetricsInput'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'

describe('CalculatePerformanceMetricsInput', () => {
  describe('CalculatePerformanceMetricsInputSchema', () => {
    // Sample closed position for testing
    const sampleClosedPosition = {
      id: 'position-123',
      status: PositionStatus.Closed,
      direction: TradeDirection.Buy,
      entryPrice: 50000,
      size: 1.5,
      openTimestamp: 1609459200000,
      exitPrice: 55000,
      closeTimestamp: 1609545600000,
      entryTrade: {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      },
      exitTrade: {
        direction: TradeDirection.Sell,
        price: 55000,
        volume: 1.5,
        timestamp: 1609545600000,
        fees: 0.001,
      },
    }

    it('should validate valid calculate performance metrics input', () => {
      const validInput = {
        positions: [sampleClosedPosition],
        initialCapital: 10000,
      }

      const result = Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with multiple positions', () => {
      const validInput = {
        positions: [
          sampleClosedPosition,
          {
            ...sampleClosedPosition,
            id: 'position-456',
            entryPrice: 52000,
            exitPrice: 48000, // Loss
          },
        ],
        initialCapital: 10000,
      }

      const result = Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with empty positions array', () => {
      const validInput = {
        positions: [],
        initialCapital: 10000,
      }

      const result = Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should reject input with non-positive initial capital', () => {
      const invalidInput = {
        positions: [sampleClosedPosition],
        initialCapital: 0,
      }

      expect(() => {
        Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(invalidInput)
      }).toThrow()

      const negativeCapitalInput = {
        positions: [sampleClosedPosition],
        initialCapital: -10000,
      }

      expect(() => {
        Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(
          negativeCapitalInput,
        )
      }).toThrow()
    })

    it('should reject input with missing required fields', () => {
      // Missing positions
      const missingPositions = {
        initialCapital: 10000,
      }

      expect(() => {
        Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(
          missingPositions as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing initialCapital
      const missingInitialCapital = {
        positions: [sampleClosedPosition],
      }

      expect(() => {
        Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(
          missingInitialCapital as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should accept input with open positions (validation happens elsewhere)', () => {
      const inputWithOpenPosition = {
        positions: [
          {
            ...sampleClosedPosition,
            status: PositionStatus.Open,
            exitPrice: undefined,
            closeTimestamp: undefined,
            exitTrade: undefined,
          },
        ],
        initialCapital: 10000,
      }

      // This should pass since the schema doesn't validate position status
      const result = Schema.decodeSync(CalculatePerformanceMetricsInputSchema)(
        inputWithOpenPosition,
      )
      expect(result).toEqual(inputWithOpenPosition)
    })
  })
})
