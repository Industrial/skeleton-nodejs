import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionStatus, PositionStatusSchema } from './PositionStatus'

describe('PositionStatus', () => {
  describe('PositionStatusSchema', () => {
    it('should validate Open status', () => {
      const result = Schema.decodeSync(PositionStatusSchema)(
        PositionStatus.Open,
      )
      expect(result).toBe(PositionStatus.Open)
    })

    it('should validate Closed status', () => {
      const result = Schema.decodeSync(PositionStatusSchema)(
        PositionStatus.Closed,
      )
      expect(result).toBe(PositionStatus.Closed)
    })

    it('should reject invalid statuses', () => {
      expect(() => {
        Schema.decodeSync(PositionStatusSchema)('invalid' as PositionStatus)
      }).toThrow()
    })
  })

  describe('PositionStatus enum', () => {
    it('should have the correct values', () => {
      expect(PositionStatus.Open).toBe('open')
      expect(PositionStatus.Closed).toBe('closed')
    })

    it('should have exactly two values', () => {
      expect(Object.keys(PositionStatus).length).toBe(2)
    })
  })
})
