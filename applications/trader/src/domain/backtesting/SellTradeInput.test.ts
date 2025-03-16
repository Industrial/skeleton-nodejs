import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { SellTradeInputSchema } from './SellTradeInput'

describe('SellTradeInput', () => {
  describe('SellTradeInputSchema', () => {
    it('should validate a valid sell trade input with required fields', () => {
      const validSellTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      const result = Schema.decodeSync(SellTradeInputSchema)(validSellTrade)
      expect(result).toEqual(validSellTrade)
    })

    it('should validate a sell trade input with all fields including optional ones', () => {
      const validSellTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
        id: 'sell-trade-123',
        metadata: {
          reason: 'signal',
          strategy: 'MovingAverageCrossover',
        },
      }

      const result = Schema.decodeSync(SellTradeInputSchema)(validSellTrade)
      expect(result).toEqual(validSellTrade)
    })

    it('should validate a sell trade input with zero fees', () => {
      const validSellTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0,
      }

      const result = Schema.decodeSync(SellTradeInputSchema)(validSellTrade)
      expect(result).toEqual(validSellTrade)
    })

    it('should reject a sell trade input with non-positive price', () => {
      const invalidSellTrade = {
        price: 0,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(invalidSellTrade)
      }).toThrow()

      const negativePriceSellTrade = {
        price: -50000,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(negativePriceSellTrade)
      }).toThrow()
    })

    it('should reject a sell trade input with non-positive volume', () => {
      const invalidSellTrade = {
        price: 50000,
        volume: 0,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(invalidSellTrade)
      }).toThrow()

      const negativeVolumeSellTrade = {
        price: 50000,
        volume: -1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(negativeVolumeSellTrade)
      }).toThrow()
    })

    it('should reject a sell trade input with non-positive timestamp', () => {
      const invalidSellTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 0,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(invalidSellTrade)
      }).toThrow()

      const negativeTimestampSellTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: -1609459200000,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(negativeTimestampSellTrade)
      }).toThrow()
    })

    it('should reject a sell trade input with negative fees', () => {
      const invalidSellTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: -0.001,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(invalidSellTrade)
      }).toThrow()
    })

    it('should reject a sell trade input with missing required fields', () => {
      // Missing price
      const missingPrice = {
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(
          missingPrice as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing volume
      const missingVolume = {
        price: 50000,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(
          missingVolume as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing timestamp
      const missingTimestamp = {
        price: 50000,
        volume: 1.5,
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(
          missingTimestamp as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a sell trade input with invalid metadata', () => {
      const invalidMetadataSellTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(SellTradeInputSchema)(
          invalidMetadataSellTrade as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
