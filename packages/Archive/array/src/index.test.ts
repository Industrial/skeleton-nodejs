import { describe, expect, it } from 'vitest'

import type { ArrayElement } from '.'
import {
  chunkArray,
  head,
  includesAll,
  isEmpty,
  tail,
  uniqueElements,
  zip,
} from '.'

describe('array', () => {
  describe('ArrayElement', () => {
    it('should correctly infer the element type of an array', () => {
      type TestArray = [number, string, boolean]
      type TestElement = ArrayElement<TestArray>
      const testValue: TestElement = 123
      expect(typeof testValue).toBe('number')
    })
  })

  describe('head', () => {
    describe('When called with an empty array', () => {
      it('should return undefined', () => {
        const array: Array<string> = []
        const result = head(array)
        expect(result).toBe(undefined)
      })
    })

    describe('When called with an array with one element', () => {
      it('should return the first element', () => {
        const array = ['a']
        const result = head(array)
        expect(result).toBe('a')
      })
    })

    describe('When called with an array with more than one element', () => {
      it('should return the first element', () => {
        const array = ['a', 'b', 'c']
        const result = head(array)
        expect(result).toBe('a')
      })
    })
  })

  describe('tail', () => {
    describe('When called with an empty array', () => {
      it('should return an empty array', () => {
        const array: Array<string> = []
        const result = tail(array)
        expect(result).toEqual([])
      })
    })

    describe('When called with an array with one element', () => {
      it('should return an empty array', () => {
        const array = ['a']
        const result = tail(array)
        expect(result).toEqual([])
      })
    })

    describe('When called with an array with more than one element', () => {
      it('should return the tail of the array', () => {
        const array = ['a', 'b', 'c']
        const result = tail(array)
        expect(result).toEqual(['b', 'c'])
      })
    })
  })

  describe('zip', () => {
    describe('When called with two empty arrays', () => {
      it('should return an empty array', () => {
        const array1: Array<string> = []
        const array2: Array<number> = []
        const result = zip(array1, array2)
        expect(result).toStrictEqual([])
      })
    })

    describe('When called with two arrays with different lengths', () => {
      it('should return undefined for each missing element', () => {
        const array1 = ['a', 'b', 'c', 'd']
        const array2 = [1, 2]
        const result = zip(array1, array2)
        expect(result).toStrictEqual([
          ['a', 1],
          ['b', 2],
          ['c', undefined],
          ['d', undefined],
        ])
      })
    })

    describe('When called with two arrays with the same length', () => {
      it('should return an array of tuples', () => {
        const array1 = ['a', 'b', 'c']
        const array2 = [1, 2, 3]
        const result = zip(array1, array2)
        expect(result).toStrictEqual([
          ['a', 1],
          ['b', 2],
          ['c', 3],
        ])
      })
    })
  })

  describe('includesAll', () => {
    describe('When called with two empty arrays', () => {
      it('should return true', () => {
        const array1: Array<string> = []
        const array2: Array<string> = []
        const result = includesAll(array1, array2)
        expect(result).toBe(true)
      })
    })

    describe('When called with two arrays with different lengths', () => {
      it('should return false', () => {
        const array1 = ['a', 'b', 'c']
        const array2 = ['d', 'e', 'f']
        const result = includesAll(array1, array2)
        expect(result).toBe(false)
      })
    })

    describe('When called with two arrays with the same length', () => {
      describe('When all elements of the second array are in the first array', () => {
        it('should return true', () => {
          const array1 = ['a', 'b', 'c']
          const array2 = ['a', 'b', 'c']
          const result = includesAll(array1, array2)
          expect(result).toBe(true)
        })
      })

      describe('When not all elements of the second array are in the first array', () => {
        it('should return false', () => {
          const array1 = ['a', 'b', 'c']
          const array2 = ['a', 'b', 'd']
          const result = includesAll(array1, array2)
          expect(result).toBe(false)
        })
      })
    })
  })

  describe('uniqueElements', () => {
    describe('When the array has no duplicates', () => {
      it('should return the same array', () => {
        const array = [1, 2, 3, 4, 5]
        const result = uniqueElements(array)
        expect(result).toEqual(array)
      })
    })

    describe('When the array has duplicates', () => {
      it('should return an array with only unique elements', () => {
        const array = [1, 2, 3, 2, 4, 5, 4]
        const result = uniqueElements(array)
        expect(result).toEqual([1, 2, 3, 4, 5])
      })
    })
  })

  describe('isEmpty', () => {
    describe('When the array is empty', () => {
      it('should return true', () => {
        const array: Array<string> = []
        const result = isEmpty(array)
        expect(result).toBe(true)
      })
    })

    describe('When the array is not empty', () => {
      it('should return false', () => {
        const array = ['a']
        const result = isEmpty(array)
        expect(result).toBe(false)
      })
    })
  })

  describe('chunkArray', () => {
    describe('When the array is empty', () => {
      it('should return an empty array of an empty array', () => {
        const array: Array<string> = []
        const result = chunkArray(array, 2)
        expect(result).toStrictEqual([[]])
      })
    })

    describe('When the array has one element', () => {
      it('should return an array with one chunk', () => {
        const array = ['a']
        const result = chunkArray(array, 2)
        expect(result).toStrictEqual([['a']])
      })
    })

    describe('When the array has multiple elements', () => {
      it('should return an array with multiple chunks', () => {
        const array = ['a', 'b', 'c', 'd']
        const result = chunkArray(array, 2)
        expect(result).toStrictEqual([
          ['a', 'b'],
          ['c', 'd'],
        ])
      })
    })
  })
})
