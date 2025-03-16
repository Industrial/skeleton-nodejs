import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionInputSchema } from './PositionInput'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'

describe('PositionInput', () => {
  describe('PositionInputSchema', () => {
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

    it('should validate a valid open position input', () => {
      const validInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      const result = Schema.decodeSync(PositionInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate a valid closed position input', () => {
      const validInput = {
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

      const result = Schema.decodeSync(PositionInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate a position input with metadata', () => {
      const validInput = {
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

      const result = Schema.decodeSync(PositionInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject a position input with non-positive entry price', () => {
      const invalidInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 0, // Invalid: non-positive
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(invalidInput)
      }).toThrow()

      const negativeEntryPriceInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: -50000, // Invalid: negative
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(negativeEntryPriceInput)
      }).toThrow()
    })

    it('should reject a position input with non-positive size', () => {
      const invalidInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 0, // Invalid: non-positive
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(invalidInput)
      }).toThrow()

      const negativeSizeInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: -1.5, // Invalid: negative
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(negativeSizeInput)
      }).toThrow()
    })

    it('should reject a position input with non-positive open timestamp', () => {
      const invalidInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 0, // Invalid: non-positive
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(invalidInput)
      }).toThrow()

      const negativeOpenTimestampInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: -1609459200000, // Invalid: negative
        entryTrade: sampleEntryTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(negativeOpenTimestampInput)
      }).toThrow()
    })

    it('should reject a closed position input with non-positive exit price', () => {
      const invalidInput = {
        id: 'position-123',
        status: PositionStatus.Closed,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        exitPrice: 0, // Invalid: non-positive
        closeTimestamp: 1609545600000,
        entryTrade: sampleEntryTrade,
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(invalidInput)
      }).toThrow()
    })

    it('should reject a closed position input with non-positive close timestamp', () => {
      const invalidInput = {
        id: 'position-123',
        status: PositionStatus.Closed,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        exitPrice: 55000,
        closeTimestamp: 0, // Invalid: non-positive
        entryTrade: sampleEntryTrade,
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(invalidInput)
      }).toThrow()
    })

    it('should reject a position input with missing required fields', () => {
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
        Schema.decodeSync(PositionInputSchema)(
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
        Schema.decodeSync(PositionInputSchema)(
          missingStatus as unknown as Record<string, unknown>,
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
        Schema.decodeSync(PositionInputSchema)(
          missingEntryTrade as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a position input with invalid metadata', () => {
      const invalidMetadataInput = {
        id: 'position-123',
        status: PositionStatus.Open,
        direction: TradeDirection.Buy,
        entryPrice: 50000,
        size: 1.5,
        openTimestamp: 1609459200000,
        entryTrade: sampleEntryTrade,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(PositionInputSchema)(
          invalidMetadataInput as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
