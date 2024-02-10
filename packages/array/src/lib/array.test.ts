import { describe, expect, it } from 'vitest'

import { firstElement } from './array.ts'

describe('array', () => {
  describe('firstElement', () => {
    describe('When called with an empty array', () => {
      it('should return undefined', () => {
        const array: Array<string> = []
        const result = firstElement(array)
        expect(result).toBe(undefined)
      })
    })

    describe('When called with an array with one element', () => {
      it('should return the first element', () => {
        const array = ['a']
        const result = firstElement(array)
        expect(result).toBe('a')
      })
    })

    describe('When called with an array with more than one element', () => {
      it('should return the first element', () => {
        const array = ['a', 'b', 'c']
        const result = firstElement(array)
        expect(result).toBe('a')
      })
    })
  })
})
