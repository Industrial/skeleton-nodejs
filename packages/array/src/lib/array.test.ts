import { describe, expect, it } from 'bun:test'
import { Effect as Fx, Option } from 'effect'

import { allIndexesOf, average, sliceE } from './array.ts'

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
})
