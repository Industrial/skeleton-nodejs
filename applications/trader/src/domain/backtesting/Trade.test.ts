import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { TradeSchema } from './Trade'
import { TradeDirection } from './TradeDirection'

describe('Trade', () => {
  describe('TradeSchema', () => {
    it('should validate a valid trade with all required fields', () => {
      const validTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      const result = Schema.decodeSync(TradeSchema)(validTrade)
      expect(result).toEqual(validTrade)
    })

    it('should validate a valid trade with all fields including optional ones', () => {
      const validTrade = {
        direction: TradeDirection.Sell,
        price: 52000,
        volume: 1.5,
        timestamp: 1609545600000,
        fees: 0.001,
        id: 'trade-123',
        metadata: {
          reason: 'signal',
          strategy: 'MovingAverageCrossover',
        },
      }

      const result = Schema.decodeSync(TradeSchema)(validTrade)
      expect(result).toEqual(validTrade)
    })

    it('should validate a trade with zero fees', () => {
      const validTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0,
      }

      const result = Schema.decodeSync(TradeSchema)(validTrade)
      expect(result).toEqual(validTrade)
    })

    it('should reject a trade with invalid direction', () => {
      const invalidTrade = {
        direction: 'invalid' as TradeDirection,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(invalidTrade)
      }).toThrow()
    })

    it('should reject a trade with non-positive price', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 0,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(invalidTrade)
      }).toThrow()

      const negativePriceTrade = {
        direction: TradeDirection.Buy,
        price: -100,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(negativePriceTrade)
      }).toThrow()
    })

    it('should reject a trade with non-positive volume', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 0,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(invalidTrade)
      }).toThrow()

      const negativeVolumeTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: -1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(negativeVolumeTrade)
      }).toThrow()
    })

    it('should reject a trade with non-positive timestamp', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 0,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(invalidTrade)
      }).toThrow()

      const negativeTimestampTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: -1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(negativeTimestampTrade)
      }).toThrow()
    })

    it('should reject a trade with negative fees', () => {
      const invalidTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: -0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(invalidTrade)
      }).toThrow()
    })

    it('should reject a trade with missing required fields', () => {
      // Missing direction
      const missingDirection = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(missingDirection as any)
      }).toThrow()

      // Missing price
      const missingPrice = {
        direction: TradeDirection.Buy,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(missingPrice as any)
      }).toThrow()

      // Missing volume
      const missingVolume = {
        direction: TradeDirection.Buy,
        price: 50000,
        timestamp: 1609459200000,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(missingVolume as any)
      }).toThrow()

      // Missing timestamp
      const missingTimestamp = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        fees: 0.001,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(missingTimestamp as any)
      }).toThrow()

      // Missing fees
      const missingFees = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(missingFees as any)
      }).toThrow()
    })

    it('should reject a trade with invalid metadata', () => {
      const invalidMetadataTrade = {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(TradeSchema)(invalidMetadataTrade as any)
      }).toThrow()
    })
  })
})
