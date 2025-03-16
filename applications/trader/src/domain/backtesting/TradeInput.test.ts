import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { TradeDirection } from './TradeDirection'
import { TradeInputSchema } from './TradeInput'

describe('TradeInput', () => {
  describe('TradeInputSchema', () => {
    it('should validate a valid trade input with required fields', () => {
      const validTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      const result = Schema.decodeSync(TradeInputSchema)(validTrade)
      expect(result).toEqual(validTrade)
    })

    it('should validate a trade input with all fields including optional ones', () => {
      const validTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
        id: 'trade-123',
        metadata: {
          reason: 'signal',
          strategy: 'MovingAverageCrossover',
        },
      }

      const result = Schema.decodeSync(TradeInputSchema)(validTrade)
      expect(result).toEqual(validTrade)
    })

    it('should validate a trade input with sell direction', () => {
      const validTrade = {
        direction: TradeDirection.Sell,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      const result = Schema.decodeSync(TradeInputSchema)(validTrade)
      expect(result).toEqual(validTrade)
    })

    it('should validate a trade input with zero fees', () => {
      const validTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0,
      }

      const result = Schema.decodeSync(TradeInputSchema)(validTrade)
      expect(result).toEqual(validTrade)
    })

    it('should reject a trade input with invalid direction', () => {
      const invalidTrade = {
        direction: 'invalid' as TradeDirection,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(invalidTrade)
      }).toThrow()
    })

    it('should reject a trade input with non-positive price', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 0,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(invalidTrade)
      }).toThrow()

      const negativePriceTrade = {
        direction: TradeDirection.Buy,
        price: -50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(negativePriceTrade)
      }).toThrow()
    })

    it('should reject a trade input with non-positive volume', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 0,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(invalidTrade)
      }).toThrow()

      const negativeVolumeTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: -1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(negativeVolumeTrade)
      }).toThrow()
    })

    it('should reject a trade input with non-positive timestamp', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 0,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(invalidTrade)
      }).toThrow()

      const negativeTimestampTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: -1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(negativeTimestampTrade)
      }).toThrow()
    })

    it('should reject a trade input with negative fees', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: -0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(invalidTrade)
      }).toThrow()
    })

    it('should reject a trade input with missing required fields', () => {
      // Missing direction
      const missingDirection = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(
          missingDirection as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing price
      const missingPrice = {
        direction: TradeDirection.Buy,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(
          missingPrice as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing volume
      const missingVolume = {
        direction: TradeDirection.Buy,
        price: 50000,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(
          missingVolume as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing timestamp
      const missingTimestamp = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(
          missingTimestamp as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing fees
      const missingFees = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(
          missingFees as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a trade input with invalid metadata', () => {
      const invalidMetadataTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(TradeInputSchema)(
          invalidMetadataTrade as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
