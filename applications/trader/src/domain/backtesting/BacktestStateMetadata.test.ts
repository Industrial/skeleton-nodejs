import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { BacktestStateMetadataSchema } from './BacktestStateMetadata'

describe('BacktestStateMetadata', () => {
  describe('BacktestStateMetadataSchema', () => {
    it('should validate empty metadata', () => {
      const emptyMetadata = {}
      const result = Schema.decodeSync(BacktestStateMetadataSchema)(
        emptyMetadata,
      )
      expect(result).toEqual(emptyMetadata)
    })

    it('should validate metadata with string values', () => {
      const metadata = {
        status: 'running',
        currentPair: 'BTC/USDT',
      }
      const result = Schema.decodeSync(BacktestStateMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with number values', () => {
      const metadata = {
        currentTimestamp: 1609459200000,
        processingSpeed: 1000, // candlesticks per second
        progress: 0.75,
      }
      const result = Schema.decodeSync(BacktestStateMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with boolean values', () => {
      const metadata = {
        isComplete: false,
        hasErrors: false,
      }
      const result = Schema.decodeSync(BacktestStateMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with object values', () => {
      const metadata = {
        currentMarketData: {
          price: 50000,
          volume: 100,
          timestamp: 1609459200000,
        },
        strategyState: {
          indicators: {
            fastMA: [10, 11, 12],
            slowMA: [9, 9.5, 10],
          },
        },
      }
      const result = Schema.decodeSync(BacktestStateMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with mixed value types', () => {
      const metadata = {
        status: 'running',
        currentTimestamp: 1609459200000,
        isComplete: false,
        currentMarketData: {
          price: 50000,
          volume: 100,
        },
      }
      const result = Schema.decodeSync(BacktestStateMetadataSchema)(metadata)
      expect(result).toEqual(metadata)
    })

    it('should reject non-object metadata', () => {
      expect(() => {
        Schema.decodeSync(BacktestStateMetadataSchema)(
          'not an object' as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(BacktestStateMetadataSchema)(
          123 as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(BacktestStateMetadataSchema)(
          null as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(BacktestStateMetadataSchema)(
          undefined as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
