import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { TradeMetadataSchema } from './TradeMetadata'

describe('TradeMetadata', () => {
  describe('TradeMetadataSchema', () => {
    it('should validate empty metadata', () => {
      const emptyMetadata = {}
      const result = Schema.decodeSync(TradeMetadataSchema)(emptyMetadata)
      expect(result).toEqual(emptyMetadata)
    })

    it('should validate metadata with string values', () => {
      const metadata = {
        source: 'strategy',
        reason: 'signal',
      }
      const result = Schema.decodeSync(TradeMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with number values', () => {
      const metadata = {
        signalStrength: 0.8,
        confidence: 0.95,
      }
      const result = Schema.decodeSync(TradeMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with boolean values', () => {
      const metadata = {
        isConfirmed: true,
        isManual: false,
      }
      const result = Schema.decodeSync(TradeMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with object values', () => {
      const metadata = {
        signal: {
          type: 'crossover',
          value: 0.5,
        },
        indicators: {
          ma: [10, 20, 30],
          rsi: 65,
        },
      }
      const result = Schema.decodeSync(TradeMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with mixed value types', () => {
      const metadata = {
        source: 'strategy',
        signalStrength: 0.8,
        isConfirmed: true,
        signal: {
          type: 'crossover',
          value: 0.5,
        },
      }
      const result = Schema.decodeSync(TradeMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should reject non-object metadata', () => {
      expect(() => {
        Schema.decodeSync(TradeMetadataSchema)('not an object')
      }).toThrow()

      expect(() => {
        Schema.decodeSync(TradeMetadataSchema)(123)
      }).toThrow()

      expect(() => {
        Schema.decodeSync(TradeMetadataSchema)(null)
      }).toThrow()

      expect(() => {
        Schema.decodeSync(TradeMetadataSchema)(undefined)
      }).toThrow()
    })
  })
})
