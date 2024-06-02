import { describe, expect, it } from 'bun:test'
import { Effect as E } from 'effect'

import { sma } from './sma.ts'

describe('sma', () => {
  describe('when calculating the Simple Moving Average', () => {
    describe('When the array is empty', () => {
      it('should throw an error', () => {
        expect(() => {
          E.runSync(sma(3)(E.succeed([])))
        }).toThrowError(new Error('Calculation failed'))
      })
    })

    describe('when having a non-empty array of numbers', () => {
      describe('when calculating the SMA for length 1', () => {
        it('should throw an error', () => {
          const values = E.succeed([1, 2, 3, 4, 5])
          expect(() => {
            E.runSync(sma(1)(values))
          }).toThrowError(new Error('Calculation failed'))
        })
      })

      describe('when calculating the SMA for length 2', () => {
        it('should calculate the SMA correctly', () => {
          const values = E.succeed([1, 2, 3, 4, 5])
          const result = E.runSync(sma(2)(values))
          expect(result).toEqual([1.5, 2.5, 3.5, 4.5, 5])
        })
      })

      describe('when calculating the SMA for length 3', () => {
        it('should calculate the SMA correctly', () => {
          const values = E.succeed([1, 2, 3, 4, 5])
          const result = E.runSync(sma(3)(values))
          expect(result).toEqual([2, 3, 4, 4.5])
        })
      })

      describe('when calculating the SMA for length 4', () => {
        it('should calculate the SMA correctly', () => {
          const values = E.succeed([1, 2, 3, 4, 5])
          const result = E.runSync(sma(4)(values))
          expect(result).toEqual([2.5, 3.5, 4])
        })
      })

      describe('when calculating the SMA for length 5', () => {
        it('should calculate the SMA correctly', () => {
          const values = E.succeed([1, 2, 3, 4, 5])
          const result = E.runSync(sma(5)(values))
          expect(result).toEqual([3, 3.5])
        })
      })
    })
  })
})
