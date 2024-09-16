import { describe, expect, it } from 'bun:test'

import { assertIsNotNull, assertIsNull, isNotNull, isNull } from './null.ts'

describe('isNull', () => {
  describe('When the value is null', () => {
    it('should return true', () => {
      const result = isNull(null)
      expect(result).toBe(true)
    })
  })

  describe('When the value is not null', () => {
    it('should return false', () => {
      const result = isNull(1)
      expect(result).toBe(false)
    })
  })
})

describe('isNotNull', () => {
  describe('When the value is null', () => {
    it('should return false', () => {
      const result = isNotNull(null)
      expect(result).toBe(false)
    })
  })

  describe('When the value is not null', () => {
    it('should return true', () => {
      const result = isNotNull(1)
      expect(result).toBe(true)
    })
  })
})

describe('assertIsNull', () => {
  describe('When the value is null', () => {
    it('should not throw an error', () => {
      expect(() => {
        assertIsNull(null)
      }).not.toThrow()
    })
  })

  describe('When the value is not null', () => {
    it('should throw an error', () => {
      expect(() => {
        assertIsNull(1)
      }).toThrow()
    })
  })

  describe('When the value is undefined', () => {
    it('should throw an error', () => {
      expect(() => {
        assertIsNull(undefined)
      }).toThrow()
    })
  })
})

describe('assertIsNotNull', () => {
  describe('When the value is not null', () => {
    it('should not throw an error', () => {
      expect(() => {
        assertIsNotNull(1)
      }).not.toThrow()
    })
  })

  describe('When the value is null', () => {
    it('should throw an error', () => {
      expect(() => {
        assertIsNotNull(null)
      }).toThrow()
    })
  })

  describe('When the value is undefined', () => {
    it('should throw an error', () => {
      expect(() => {
        assertIsNotNull(undefined)
      }).toThrow()
    })
  })
})
