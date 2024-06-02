import { describe, expect, it } from 'bun:test'
import { Effect as E } from 'effect'

import { ema } from './ema.ts'

describe('ema', () => {
  describe('When no values are provided', () => {
    it('should return an empty array', () => {
      const actual = E.runSync(ema(5)(E.succeed([])))
      const expected: Array<number> = []
      expect(actual).toStrictEqual(expected)
    })
  })

  describe('When values are provided', () => {
    describe('When the first value is defined', () => {
      describe('When the length is zero', () => {
        it('should return an array with the first value', () => {
          const actual = E.runSync(ema(0)(E.succeed([1])))
          const expected: Array<number> = [1]
          expect(actual).toStrictEqual(expected)
        })
      })

      describe('When the length is greater than zero', () => {
        describe('When the first argument is undefined', () => {
          it('should return an empty array', () => {
            const actual = E.runSync(ema(5)(E.succeed([undefined as unknown as number, 1, 2, 3])))
            const expected: Array<number> = []
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the alpha function is not provided', () => {
          it('should return an array with the exponential moving average', () => {
            const actual = E.runSync(ema(5)(E.succeed([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])))
            const expected: Array<number> = [
              1, 1.3333333333333335, 1.888888888888889, 2.5925925925925926, 3.3950617283950617, 4.263374485596708,
              5.175582990397805, 6.117055326931871, 7.078036884621247, 8.052024589747498,
            ]
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the alpha function is provided', () => {
          it('should return an array with the exponential moving average using the provided alpha function', () => {
            const alpha = (length: number): number =>
              1 / length
            const actual = E.runSync(ema(5, alpha)(E.succeed([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])))
            const expected: Array<number> = [
              1,
              1.2000000000000002,
              1.5600000000000003,
              2.048,
              2.6384,
              3.3107200000000003,
              4.048576000000001,
              4.838860800000001,
              5.671088640000001,
              6.536870912,
            ]
            expect(actual).toStrictEqual(expected)
          })
        })
      })
    })
  })
})
