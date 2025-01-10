import { describe, expect, it } from 'vitest'

import { isNotNull, isNull } from '.'

describe('null', () => {
  describe('isNull', () => {
    describe('When the value is null', () => {
      it('should return true', () => {
        const value: string | null = null
        const result = isNull(value)
        expect(result).toBe(true)
      })
    })

    describe('When the value is not null', () => {
      it('should return false', () => {
        const value: string | null = 'a'
        const result = isNull(value)
        expect(result).toBe(false)
      })
    })
  })

  describe('isNotNull', () => {
    describe('When the value is null', () => {
      it('should return false', () => {
        const value: string | null = null
        const result = isNotNull(value)
        expect(result).toBe(false)
      })
    })

    describe('When the value is not null', () => {
      it('should return true', () => {
        const value: string | null = 'a'
        const result = isNotNull(value)
        expect(result).toBe(true)
      })
    })
  })
})
