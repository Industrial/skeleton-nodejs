import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { BacktestParametersMetadataSchema } from './BacktestParametersMetadata'

describe('BacktestParametersMetadata', () => {
  describe('BacktestParametersMetadataSchema', () => {
    it('should validate empty metadata', () => {
      const emptyMetadata = {}
      const result = Schema.decodeSync(BacktestParametersMetadataSchema)(
        emptyMetadata,
      )
      expect(result).toEqual(emptyMetadata)
    })

    it('should validate metadata with string values', () => {
      const metadata = {
        description: 'Backtest for BTC/USDT with MA Crossover',
        market: 'crypto',
      }
      const result = Schema.decodeSync(BacktestParametersMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with number values', () => {
      const metadata = {
        maxDrawdownThreshold: 0.2,
        targetAnnualReturn: 0.3,
        riskFreeRate: 0.02,
      }
      const result = Schema.decodeSync(BacktestParametersMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with boolean values', () => {
      const metadata = {
        useMarketHours: true,
        allowShortSelling: false,
      }
      const result = Schema.decodeSync(BacktestParametersMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with object values', () => {
      const metadata = {
        strategyParameters: {
          fastPeriod: 10,
          slowPeriod: 20,
          priceType: 'close',
        },
        marketData: {
          source: 'binance',
          timeframe: '1h',
          pair: 'BTC/USDT',
        },
      }
      const result = Schema.decodeSync(BacktestParametersMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should validate metadata with mixed value types', () => {
      const metadata = {
        description: 'Backtest for BTC/USDT with MA Crossover',
        maxDrawdownThreshold: 0.2,
        useMarketHours: true,
        strategyParameters: {
          fastPeriod: 10,
          slowPeriod: 20,
        },
      }
      const result = Schema.decodeSync(BacktestParametersMetadataSchema)(
        metadata,
      )
      expect(result).toEqual(metadata)
    })

    it('should reject non-object metadata', () => {
      expect(() => {
        Schema.decodeSync(BacktestParametersMetadataSchema)(
          'not an object' as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(BacktestParametersMetadataSchema)(
          123 as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(BacktestParametersMetadataSchema)(
          null as unknown as Record<string, unknown>,
        )
      }).toThrow()

      expect(() => {
        Schema.decodeSync(BacktestParametersMetadataSchema)(
          undefined as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
