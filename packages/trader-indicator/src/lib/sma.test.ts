import { describe, expect, test } from 'bun:test'

import { sma } from './sma.ts'

describe('sma', () => {
  describe('When passed a length of 2 and an Array with the numbers 1 to 10', () => {
    test('Should return the correct sequence', () => {
      const input = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      const length = 2
      const expected = [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5]
      const actual = sma(length, input)
      expect(actual).toEqual(expected)
    })
  })
})
