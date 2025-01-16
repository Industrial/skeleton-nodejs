import { describe, expect, it } from 'bun:test'
import { Effect as Fx, Option } from 'effect'

import {
  type ArrayElement,
  allIndexesOf,
  average,
  chunkArray,
  head,
  includesAll,
  isEmpty,
  sliceE,
  tail,
  uniqueElements,
  zip,
} from './array.ts'

describe('array', () => {
  describe('sliceE', () => {
    describe('When start is undefined', () => {
      describe('When end is undefined', () => {
        it('should return the entire array', () => {
          const result = sliceE(undefined, undefined)([1, 2, 3])
          expect(Fx.runSync(result)).toEqual([1, 2, 3])
        })
      })

      describe('When end is defined', () => {
        it('should slice the array from the beginning to the end index', () => {
          const result = sliceE(undefined, 2)([1, 2, 3])
          expect(Fx.runSync(result)).toEqual([1, 2])
        })
      })
    })

    describe('When start is defined', () => {
      describe('When end is undefined', () => {
        it('should slice the array from the start index to the array length', () => {
          const result = sliceE(1, undefined)([1, 2, 3])
          expect(Fx.runSync(result)).toEqual([2, 3])
        })
      })

      describe('When end is defined', () => {
        describe('When start is less than end', () => {
          describe('When start and end are within bounds', () => {
            it('should slice the array from the start index to the end index', () => {
              const result = sliceE(1, 2)([1, 2, 3])
              expect(Fx.runSync(result)).toEqual([2])
            })
          })

          describe('When start is out of bounds', () => {
            it('should return an empty array', () => {
              const result = sliceE(10, 20)([1, 2, 3])
              expect(Fx.runSync(result)).toEqual([])
            })
          })

          describe('When end is out of bounds', () => {
            it('should slice the array from the start index to the array length', () => {
              const result = sliceE(1, 10)([1, 2, 3])
              expect(Fx.runSync(result)).toEqual([2, 3])
            })
          })
        })

        describe('When start is greater than or equal to end', () => {
          it('should return an empty array', () => {
            const result = sliceE(3, 2)([1, 2, 3])
            expect(Fx.runSync(result)).toEqual([])
          })
        })
      })
    })
  })

  describe('allIndexesOf', () => {
    describe('When the array is empty', () => {
      it('should return an empty array', () => {
        const result = allIndexesOf(42, [])
        expect(result).toEqual([])
      })
    })

    describe('When the array contains elements', () => {
      describe('When the item is present in the array', () => {
        describe('When the item appears once', () => {
          it('should return an array with a single index', () => {
            const result = allIndexesOf(42, [42])
            expect(result).toEqual([0])
          })
        })

        describe('When the item appears multiple times', () => {
          it('should return an array with all indexes of the item', () => {
            const result = allIndexesOf(2, [1, 2, 3, 2, 4, 2, 5])
            expect(result).toEqual([1, 3, 5])
          })
        })
      })

      describe('When the item is not present in the array', () => {
        it('should return an empty array', () => {
          const result = allIndexesOf(42, [1, 2, 3, 4, 5])
          expect(result).toEqual([])
        })
      })
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
        expect(Option.getOrElse(() => 0)(result)).toBe(1)
      })
    })

    describe('When the array has multiple elements', () => {
      describe('When the elements are all positive numbers', () => {
        it('should return the correct average', () => {
          const result = average([1, 2, 3])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() => 0)(result)).toBe(2)
        })
      })

      describe('When the elements include negative numbers', () => {
        it('should return the correct average', () => {
          const result = average([-1, -2, -3])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() => 0)(result)).toBe(-2)
        })
      })

      describe('When the elements are all negative numbers', () => {
        it('should return the correct average', () => {
          const result = average([-1, -2, -3])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() => 0)(result)).toBe(-2)
        })
      })

      describe('When the elements include zero', () => {
        it('should return the correct average', () => {
          const result = average([0, 0, 0])
          expect(Option.isSome(result)).toBe(true)
          expect(Option.getOrElse(() => 0)(result)).toBe(0)
        })
      })
    })

    describe('When the array contains floating point numbers', () => {
      it('should return the correct average', () => {
        const result = average([1.5, 2.5, 3.5])
        expect(Option.isSome(result)).toBe(true)
        expect(Option.getOrElse(() => 0)(result)).toBe(2.5)
      })
    })
  })

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
        expect(result).toBeUndefined()
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
        const result: [string, number | undefined][] = zip(array1, array2)
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
