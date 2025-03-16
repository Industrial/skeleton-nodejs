import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionMetadataSchema } from './PositionMetadata'

describe('PositionMetadata', () => {
  describe('PositionMetadataSchema', () => {
    it('should validate empty metadata', () => {
      const emptyMetadata = {}
      const result = Schema.decodeSync(PositionMetadataSchema)(emptyMetadata)
      expect(result).toEqual(emptyMetadata)
    })

    it('should validate metadata with string values', () => {
      const metadata = {
        strategy: 'MovingAverageCrossover',
        notes: 'Strong signal',
      }
      const result = Schema.decodeSync(PositionMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with number values', () => {
      const metadata = {
        stopLoss: 9500,
        takeProfit: 11000,
        riskRewardRatio: 2.5,
      }
      const result = Schema.decodeSync(PositionMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with boolean values', () => {
      const metadata = {
        isHedged: true,
        isLeveraged: false,
      }
      const result = Schema.decodeSync(PositionMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with object values', () => {
      const metadata = {
        strategy: {
          name: 'MovingAverageCrossover',
          parameters: {
            fastPeriod: 10,
            slowPeriod: 20,
          },
        },
        riskManagement: {
          stopLoss: 9500,
          takeProfit: 11000,
        },
      }
      const result = Schema.decodeSync(PositionMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with mixed value types', () => {
      const metadata = {
        strategy: 'MovingAverageCrossover',
        stopLoss: 9500,
        isHedged: true,
        riskManagement: {
          stopLossPercentage: 0.05,
          takeProfitPercentage: 0.1,
        },
      }
      const result = Schema.decodeSync(PositionMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should reject non-object metadata', () => {
      expect(() => {
        Schema.decodeSync(PositionMetadataSchema)('not an object' as any)
      }).toThrow()

      expect(() => {
        Schema.decodeSync(PositionMetadataSchema)(123 as any)
      }).toThrow()

      expect(() => {
        Schema.decodeSync(PositionMetadataSchema)(null as any)
      }).toThrow()

      expect(() => {
        Schema.decodeSync(PositionMetadataSchema)(undefined as any)
      }).toThrow()
    })
  })
})
