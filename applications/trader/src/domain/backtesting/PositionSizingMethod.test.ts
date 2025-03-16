import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import {
  PositionSizingMethod,
  PositionSizingMethodSchema,
} from './PositionSizingMethod'

describe('PositionSizingMethod', () => {
  describe('PositionSizingMethodSchema', () => {
    it('should validate Fixed method', () => {
      const result = Schema.decodeSync(PositionSizingMethodSchema)(
        PositionSizingMethod.Fixed,
      )
      expect(result).toBe(PositionSizingMethod.Fixed)
    })

    it('should validate PercentageOfCapital method', () => {
      const result = Schema.decodeSync(PositionSizingMethodSchema)(
        PositionSizingMethod.PercentageOfCapital,
      )
      expect(result).toBe(PositionSizingMethod.PercentageOfCapital)
    })

    it('should validate RiskBased method', () => {
      const result = Schema.decodeSync(PositionSizingMethodSchema)(
        PositionSizingMethod.RiskBased,
      )
      expect(result).toBe(PositionSizingMethod.RiskBased)
    })

    it('should validate Kelly method', () => {
      const result = Schema.decodeSync(PositionSizingMethodSchema)(
        PositionSizingMethod.Kelly,
      )
      expect(result).toBe(PositionSizingMethod.Kelly)
    })

    it('should reject invalid methods', () => {
      expect(() => {
        Schema.decodeSync(PositionSizingMethodSchema)(
          'invalid' as PositionSizingMethod,
        )
      }).toThrow()
    })
  })

  describe('PositionSizingMethod enum', () => {
    it('should have the correct values', () => {
      expect(PositionSizingMethod.Fixed).toBe('fixed')
      expect(PositionSizingMethod.PercentageOfCapital).toBe(
        'percentageOfCapital',
      )
      expect(PositionSizingMethod.RiskBased).toBe('riskBased')
      expect(PositionSizingMethod.Kelly).toBe('kelly')
    })

    it('should have exactly four values', () => {
      expect(Object.keys(PositionSizingMethod).length).toBe(4)
    })
  })
})
