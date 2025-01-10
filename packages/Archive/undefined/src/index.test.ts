import { describe, expect, it } from 'vitest'

import { isNotUndefined, isUndefined } from '.'

describe('undefined', () => {
  describe('isUndefined', () => {
    describe('When the value is undefined', () => {
      it('should return true', () => {
        const value: string | undefined = undefined
        const result = isUndefined(value)
        expect(result).toBe(true)
      })
    })

    describe('When the value is not undefined', () => {
      it('should return false', () => {
        const value: string | undefined = 'a'
        const result = isUndefined(value)
        expect(result).toBe(false)
      })
    })
  })

  describe('isNotUndefined', () => {
    describe('When the value is undefined', () => {
      it('should return false', () => {
        const value: string | undefined = undefined
        const result = isNotUndefined(value)
        expect(result).toBe(false)
      })
    })

    describe('When the value is not undefined', () => {
      it('should return true', () => {
        const value: string | undefined = 'a'
        const result = isNotUndefined(value)
        expect(result).toBe(true)
      })
    })
  })
})
