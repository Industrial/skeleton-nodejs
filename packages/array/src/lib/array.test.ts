import { describe, expect, it } from 'bun:test'
import { Either, pipe } from 'effect'

import { allIndexesOf, atIndexes, average } from './array.ts'

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
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.right([]))
    })

    it('should return items at specified indexes', () => {
      const indexes = [1, 0]
      const array = [10, 20, 30]
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.right([20, 10]))
    })

    it('should return an error for out-of-bounds indexes', () => {
      const indexes = [1, 3]
      const array = [1, 2]
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.left('Index out of bounds'))
    })

    it('should handle negative indexes as out-of-bounds', () => {
      const indexes = [-1, -2]
      const array = [1, 2, 3]
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.left('Index out of bounds'))
    })

    it('should return items in the original order', () => {
      const indexes = [2, 0, 1]
      const array = [10, 20, 30]
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.right([30, 10, 20]))
    })

    it('should handle duplicate indexes', () => {
      const indexes = [1, 1, 0]
      const array = [10, 20, 30]
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.right([20, 20, 10]))
    })

    it('should handle an empty array', () => {
      const indexes = [1, 2]
      const array: Array<number> = []
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.left('Index out of bounds'))
    })

    it('should return an error for negative indexes', () => {
      const indexes = [-1, -2]
      const array = [1, 2, 3]
      const result = pipe(
        array,
        atIndexes(indexes),
      )

      expect(result).toStrictEqual(Either.left('Index out of bounds'))
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
})
