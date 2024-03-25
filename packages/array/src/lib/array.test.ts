import * as E from 'fp-ts/Either'
import { describe, expect, it } from 'vitest'

import { allIndexesOf, atIndexes, average, firstElement } from './array.ts'

describe('array', () => {
  describe('allIndexesOf', () => {
    it('should return an empty array for an empty input array', () => {
      const result = allIndexesOf(42, [])
      expect(result).toEqual([])
    })

    it('should return an empty array when the item is not found', () => {
      const array = [1, 2, 3, 4, 5]
      const result = allIndexesOf(42, array)
      expect(result).toEqual([])
    })

    it('should return an array of indexes when the item is found multiple times', () => {
      const array = [1, 2, 3, 2, 4, 2, 5]
      const result = allIndexesOf(2, array)
      expect(result).toEqual([1, 3, 5])
    })

    it('should return an array of indexes when the item is found at the beginning of the array', () => {
      const array = [42, 1, 2, 3, 4, 5]
      const result = allIndexesOf(42, array)
      expect(result).toEqual([0])
    })

    it('should return an array of indexes when the item is found at the end of the array', () => {
      const array = [1, 2, 3, 4, 5, 42]
      const result = allIndexesOf(42, array)
      expect(result).toEqual([5])
    })

    it('should return an array of indexes when the item is found at both ends of the array', () => {
      const array = [42, 1, 2, 3, 4, 5, 42]
      const result = allIndexesOf(42, array)
      expect(result).toEqual([0, 6])
    })

    it('should return an array with a single index when the item is found only once', () => {
      const array = [1, 2, 3, 4, 5, 6]
      const result = allIndexesOf(3, array)
      expect(result).toEqual([2])
    })
  })

  describe('atIndexes', () => {
    it('should return an empty array for an empty index list', () => {
      const indexes: Array<number> = []
      const array = [1, 2, 3]
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.right([]))
    })

    it('should return items at specified indexes', () => {
      const indexes = [1, 0]
      const array = [10, 20, 30]
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.right([20, 10]))
    })

    it('should return an error for out-of-bounds indexes', () => {
      const indexes = [1, 3]
      const array = [1, 2]
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.left(new Error('Index out of bounds: 3')))
    })

    it('should handle negative indexes as out-of-bounds', () => {
      const indexes = [-1, -2]
      const array = [1, 2, 3]
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.left(new Error('Index out of bounds: -1')))
    })

    it('should return items in the original order', () => {
      const indexes = [2, 0, 1]
      const array = [10, 20, 30]
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.right([30, 10, 20]))
    })

    it('should handle duplicate indexes', () => {
      const indexes = [1, 1, 0]
      const array = [10, 20, 30]
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.right([20, 20, 10]))
    })

    it('should handle an empty array', () => {
      const indexes = [1, 2]
      const array: Array<number> = []
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.left(new Error('Index out of bounds: 1')))
    })

    it('should return an error for negative indexes', () => {
      const indexes = [-1, -2]
      const array = [1, 2, 3]
      const result = atIndexes(indexes, array)

      expect(result).toEqual(E.left(new Error('Index out of bounds: -1')))
    })
  })

  describe('average', () => {
    describe('When passed an array of 1 to 10', () => {
      it('Should return 5.5', () => {
        // Arrange
        const expected = 5.5

        // Act
        const actual = average([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

        // Assert
        expect(actual).toEqual(expected)
      })
    })
  })

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
