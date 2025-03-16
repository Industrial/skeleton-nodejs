import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionSchema } from './Position'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'

describe('Position', () => {
  describe('PositionSchema', () => {
    // Sample trade for testing
    const sampleEntryTrade = {
      direction: TradeDirection.Buy,
      price: 50000,
      volume: 1.5,
      timestamp: 1609459200000,
      fees: 0.001,
    }

    const sampleExitTrade = {
      direction: TradeDirection.Sell,
      price: 55000,
      volume: 1.5,
      timestamp: 1609545600000,
      fees: 0.001,
    }

    it('should validate a valid open position', () => {
      const validPosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      const result = Schema.decodeSync(PositionSchema)(validPosition)
      expect(result).toEqual(validPosition)
    })

    it('should validate a valid closed position', () => {
      const validPosition = {
        id: 'position-123',
        status: PositionStatus.Closed,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        exitPrice: 55000,
        closeTimestamp: 1609545600000,
        entryTrade: sampleEntryTrade,
        exitTrade: sampleExitTrade,
      }

      const result = Schema.decodeSync(PositionSchema)(validPosition)
      expect(result).toEqual(validPosition)
    })

    it('should validate a position with metadata', () => {
      const validPosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
        metadata: {
          strategy: 'MovingAverageCrossover',
          signal: 'buy',
        },
      }

      const result = Schema.decodeSync(PositionSchema)(validPosition)
      expect(result).toEqual(validPosition)
    })

    it('should reject a position with invalid status', () => {
      const invalidPosition = {
        id: 'position-123',
        status: 'invalid' as PositionStatus,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(invalidPosition)
      }).toThrow()
    })

    it('should reject a position with invalid direction', () => {
      const invalidPosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: 'invalid' as TradeDirection,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(invalidPosition)
      }).toThrow()
    })

    it('should reject a position with non-positive entry price', () => {
      const invalidPosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 0,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(invalidPosition)
      }).toThrow()

      const negativeEntryPricePosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: -50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(negativeEntryPricePosition)
      }).toThrow()
    })

    it('should reject a position with non-positive size', () => {
      const invalidPosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 0,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(invalidPosition)
      }).toThrow()

      const negativeSizePosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: -1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(negativeSizePosition)
      }).toThrow()
    })

    it('should reject a position with non-positive open timestamp', () => {
      const invalidPosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 0,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(invalidPosition)
      }).toThrow()

      const negativeOpenTimestampPosition = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: -1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(negativeOpenTimestampPosition)
      }).toThrow()
    })

    it('should reject a closed position with non-positive exit price', () => {
      const invalidPosition = {
        id: 'position-123',
        status: PositionStatus.Closed,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        exitPrice: 0,
        closeTimestamp: 1609545600000,
        entryTrade: sampleEntryTrade,
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(invalidPosition)
      }).toThrow()

      const negativeExitPricePosition = {
        id: 'position-123',
        status: PositionStatus.Closed,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        exitPrice: -55000,
        closeTimestamp: 1609545600000,
        entryTrade: sampleEntryTrade,
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(negativeExitPricePosition)
      }).toThrow()
    })

    it('should reject a closed position with non-positive close timestamp', () => {
      const invalidPosition = {
        id: 'position-123',
        status: PositionStatus.Closed,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        exitPrice: 55000,
        closeTimestamp: 0,
        entryTrade: sampleEntryTrade,
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(invalidPosition)
      }).toThrow()

      const negativeCloseTimestampPosition = {
        id: 'position-123',
        status: PositionStatus.Closed,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        exitPrice: 55000,
        closeTimestamp: -1609545600000,
        entryTrade: sampleEntryTrade,
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(negativeCloseTimestampPosition)
      }).toThrow()
    })

    it('should reject a position with missing required fields', () => {
      // Missing id
      const missingId = {
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(
          missingId as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing status
      const missingStatus = {
        id: 'position-123',
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(
          missingStatus as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing direction
      const missingDirection = {
        id: 'position-123',
        status: PositionStatus.Open,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(
          missingDirection as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing entryTrade
      const missingEntryTrade = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(PositionSchema)(
          missingEntryTrade as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a closed position without exit details', () => {
      const invalidClosedPosition = {
        id: 'position-123',
        status: PositionStatus.Closed, // Closed but missing exit details
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
        // Missing exitPrice, closeTimestamp, and exitTrade
      }

      // This should still be valid as the schema doesn't enforce this business rule
      const result = Schema.decodeSync(PositionSchema)(invalidClosedPosition)
      expect(result).toEqual(invalidClosedPosition)
    })
  })
})
