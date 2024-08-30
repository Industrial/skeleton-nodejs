import { describe, expect, it } from 'bun:test'
import { Option } from 'effect'

import { allIndexesOf, average } from './array.ts'

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

  describe('average', () => {
    describe('When the array is empty', () => {
      it('should return NaN', () => {
        const result = average([])
        expect(Option.isNone(result)).toBe(true)
      })
    })

    describe('When the array has one element', () => {
      it('should return the value of that element', () => {
        const result = average([1])
        expect(Option.isSome(result)).toBe(true)
        expect(Option.getOrElse(() =>
          0)(result)).toBe(1)
      })
    })

    describe('When the array has multiple elements', () => {
      describe('When the elements are all positive numbers', () => {
        it('should return the correct average', () => {
          const result = average([1, 2, 3])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() =>
            0)(result)).toBe(2)
        })
      })

      describe('When the elements include negative numbers', () => {
        it('should return the correct average', () => {
          const result = average([-1, -2, -3])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() =>
            0)(result)).toBe(-2)
        })
      })

      describe('When the elements are all negative numbers', () => {
        it('should return the correct average', () => {
          const result = average([-1, -2, -3])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() =>
            0)(result)).toBe(-2)
        })
      })

      describe('When the elements include zero', () => {
        it('should return the correct average', () => {
          const result = average([0, 0, 0])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() =>
            0)(result)).toBe(0)
        })
      })
    })

    describe('When the array contains floating point numbers', () => {
      it('should return the correct average', () => {
        const result = average([1.5, 2.5, 3.5])
        expect(Option.isSome(result)).toBe(true)
        expect(Option.getOrElse(() =>
          0)(result)).toBe(2.5)
      })
    })
  })
})
