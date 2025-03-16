import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { TradeDirection, TradeDirectionSchema } from './TradeDirection'

describe('TradeDirection', () => {
  describe('TradeDirectionSchema', () => {
    it('should validate Buy direction', () => {
      const result = Schema.decodeSync(TradeDirectionSchema)(TradeDirection.Buy)
      expect(result).toBe(TradeDirection.Buy)
    })

    it('should validate Sell direction', () => {
      const result = Schema.decodeSync(TradeDirectionSchema)(
        TradeDirection.Sell,
      )
      expect(result).toBe(TradeDirection.Sell)
    })

    it('should reject invalid directions', () => {
      expect(() => {
        Schema.decodeSync(TradeDirectionSchema)('invalid' as TradeDirection)
      }).toThrow()
    })
  })

  describe('TradeDirection enum', () => {
    it('should have the correct values', () => {
      expect(TradeDirection.Buy).toBe('buy')
      expect(TradeDirection.Sell).toBe('sell')
    })

    it('should have exactly two values', () => {
      expect(Object.keys(TradeDirection).length).toBe(2)
    })
  })
})
