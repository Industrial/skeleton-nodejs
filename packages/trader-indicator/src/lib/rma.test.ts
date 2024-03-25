import { describe, expect, test } from 'vitest'

import { rma } from './rma.ts'

describe('rma', () => {
  describe('When passed a length of 5 and an array of 1 to 10', () => {
    test('Should return the correct sequence', () => {
      // Arrange
      const expected = [
        1, 1.2000000000000002, 1.5600000000000003, 2.048, 2.6384, 3.3107200000000003, 4.048576000000001,
        4.838860800000001, 5.671088640000001, 6.536870912,
      ]

      // Act
      const actual = rma(5, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

      // Assert
      expect(actual).toEqual(expected)
    })
  })
})
