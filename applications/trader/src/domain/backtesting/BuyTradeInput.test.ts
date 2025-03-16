import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { BuyTradeInputSchema } from './BuyTradeInput'

describe('BuyTradeInput', () => {
  describe('BuyTradeInputSchema', () => {
    it('should validate a valid buy trade input with required fields', () => {
      const validBuyTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      const result = Schema.decodeSync(BuyTradeInputSchema)(validBuyTrade)
      expect(result).toEqual(validBuyTrade)
    })

    it('should validate a buy trade input with all fields including optional ones', () => {
      const validBuyTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
        id: 'buy-trade-123',
        metadata: {
          reason: 'signal',
          strategy: 'MovingAverageCrossover',
        },
      }

      const result = Schema.decodeSync(BuyTradeInputSchema)(validBuyTrade)
      expect(result).toEqual(validBuyTrade)
    })

    it('should validate a buy trade input with zero fees', () => {
      const validBuyTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0,
      }

      const result = Schema.decodeSync(BuyTradeInputSchema)(validBuyTrade)
      expect(result).toEqual(validBuyTrade)
    })

    it('should reject a buy trade input with non-positive price', () => {
      const invalidBuyTrade = {
        price: 0,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(invalidBuyTrade)
      }).toThrow()

      const negativePriceBuyTrade = {
        price: -50000,
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(negativePriceBuyTrade)
      }).toThrow()
    })

    it('should reject a buy trade input with non-positive volume', () => {
      const invalidBuyTrade = {
        price: 50000,
        volume: 0,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(invalidBuyTrade)
      }).toThrow()

      const negativeVolumeBuyTrade = {
        price: 50000,
        volume: -1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(negativeVolumeBuyTrade)
      }).toThrow()
    })

    it('should reject a buy trade input with non-positive timestamp', () => {
      const invalidBuyTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 0,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(invalidBuyTrade)
      }).toThrow()

      const negativeTimestampBuyTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: -1609459200000,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(negativeTimestampBuyTrade)
      }).toThrow()
    })

    it('should reject a buy trade input with negative fees', () => {
      const invalidBuyTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: -0.001,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(invalidBuyTrade)
      }).toThrow()
    })

    it('should reject a buy trade input with missing required fields', () => {
      // Missing price
      const missingPrice = {
        volume: 1.5,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(
          missingPrice as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing volume
      const missingVolume = {
        price: 50000,
        timestamp: 1609459200000,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(
          missingVolume as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing timestamp
      const missingTimestamp = {
        price: 50000,
        volume: 1.5,
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(
          missingTimestamp as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject a buy trade input with invalid metadata', () => {
      const invalidMetadataBuyTrade = {
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(BuyTradeInputSchema)(
          invalidMetadataBuyTrade as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
