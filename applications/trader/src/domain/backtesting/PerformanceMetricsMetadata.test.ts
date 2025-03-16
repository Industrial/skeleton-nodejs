import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PerformanceMetricsMetadataSchema } from './PerformanceMetricsMetadata'

describe('PerformanceMetricsMetadata', () => {
  describe('PerformanceMetricsMetadataSchema', () => {
    it('should validate empty metadata', () => {
      const emptyMetadata = {}
      const result = Schema.decodeSync(PerformanceMetricsMetadataSchema)(
        emptyMetadata,
      )
      expect(result).toEqual(emptyMetadata)
    })

    it('should validate metadata with string values', () => {
      const metadata = {
        strategy: 'MovingAverageCrossover',
        period: '2021-01-01 to 2021-12-31',
      }
      const result = Schema.decodeSync(PerformanceMetricsMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with number values', () => {
      const metadata = {
        calmarRatio: 1.5,
        ulcerIndex: 0.8,
        informationRatio: 0.6,
      }
      const result = Schema.decodeSync(PerformanceMetricsMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with boolean values', () => {
      const metadata = {
        includesCommissions: true,
        includesSlippage: true,
      }
      const result = Schema.decodeSync(PerformanceMetricsMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with object values', () => {
      const metadata = {
        monthlyReturns: {
          '2021-01': 0.05,
          '2021-02': -0.02,
          '2021-03': 0.08,
        },
        riskMetrics: {
          valueAtRisk: 0.02,
          conditionalValueAtRisk: 0.03,
        },
      }
      const result = Schema.decodeSync(PerformanceMetricsMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with mixed value types', () => {
      const metadata = {
        strategy: 'MovingAverageCrossover',
        calmarRatio: 1.5,
        includesCommissions: true,
        monthlyReturns: {
          '2021-01': 0.05,
          '2021-02': -0.02,
          '2021-03': 0.08,
        },
      }
      const result = Schema.decodeSync(PerformanceMetricsMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should reject non-object metadata', () => {
      expect(() => {
        Schema.decodeSync(PerformanceMetricsMetadataSchema)(
          'not an object' as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(PerformanceMetricsMetadataSchema)(
          123 as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(PerformanceMetricsMetadataSchema)(
          null as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(PerformanceMetricsMetadataSchema)(
          undefined as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
