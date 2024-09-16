import { describe, expect, it, test } from 'bun:test'
import { Effect as Fx, Either, Option } from 'effect'

import {
  decimalPlaces,
  factorial,
  factorialByIteration,
  getPrecision,
  getRandomNumber,
  isInteger,
  isScientificNotation,
  safeDivide,
  scientificDecimalPlaces,
} from './number.ts'

describe('number', () => {
  describe('factorial', () => {
    describe('When n is 0', () => {
      it('should return 1', () => {
        expect(factorial(0)).toBe(1)
      })
    })
    describe('When n is a positive number', () => {
      it('should return the factorial of the number', () => {
        expect(factorial(6)).toBe(720)
      })
    })
  })

  describe('factorialByIteration', () => {
    describe('When maxCount is 0', () => {
      const maxCount = 0

      describe('When counter is 0', () => {
        const counter = 0

        describe('When product is 0', () => {
          it('should return 0', () => {
            expect(factorialByIteration(maxCount, counter, 0)).toBe(0)
          })
        })
        describe('When product is above 1', () => {
          it('should return 0', () => {
            expect(factorialByIteration(maxCount, counter, 1)).toBe(0)
          })
        })
      })

      describe('When counter is 1', () => {
        const counter = 1

        describe('When product is 0', () => {
          it('should return 0', () => {
            expect(factorialByIteration(maxCount, counter, 0)).toBe(0)
          })
        })
        describe('When product is above 1', () => {
          it('should return 1', () => {
            expect(factorialByIteration(maxCount, counter, 1)).toBe(1)
          })
        })
      })
    })

    describe('When maxCount is 1', () => {
      const maxCount = 1

      describe('When counter is 0', () => {
        const counter = 0

        describe('When product is 0', () => {
          it('should return 0', () => {
            expect(factorialByIteration(maxCount, counter, 0)).toBe(0)
          })
        })
        describe('When product is above 1', () => {
          it('should return 1', () => {
            expect(factorialByIteration(maxCount, counter, 1)).toBe(0)
          })
        })
      })

      describe('When counter is 1', () => {
        const counter = 1

        describe('When product is 0', () => {
          it('should return 0', () => {
            expect(factorialByIteration(maxCount, counter, 0)).toBe(0)
          })
        })
        describe('When product is above 1', () => {
          it('should return 1', () => {
            expect(factorialByIteration(maxCount, counter, 1)).toBe(1)
          })
        })
      })
    })

    describe('When maxCount is 1, counter is 1 and product is 6', () => {
      it('should return 720', () => {
        expect(factorialByIteration(6, 1, 1)).toBe(720)
      })
    })
  })

  describe('safeDivide', () => {
    test('should divide two numbers when the denominator is not zero', () => {
      const result = safeDivide(6, 2)
      expect(Option.isSome(result)).toBe(true)
      expect(Option.getOrElse(() => 0)(result)).toBe(3)
    })

    test('should return none when the denominator is zero', () => {
      const result = safeDivide(6, 0)
      expect(Option.isNone(result)).toBe(true)
    })

    test('should return none when both numbers are zero', () => {
      const result = safeDivide(0, 0)
      expect(Option.isNone(result)).toBe(true)
    })

    test('should handle negative numbers properly', () => {
      const result = safeDivide(-6, 2)
      expect(Option.isSome(result)).toBe(true)
      expect(Option.getOrElse(() => 0)(result)).toBe(-3)
    })
  })

  describe('getRandomNumber', () => {
    describe('When the minimum number is below 0', () => {
      describe('When the maximum number is below 0', () => {
        it('should return the correct result', () => {
          const minimum = -10
          const maximum = -1
          const actual = Fx.runSync(getRandomNumber(minimum, maximum))
          expect(actual).toBeGreaterThanOrEqual(minimum)
          expect(actual).toBeLessThanOrEqual(maximum)
        })
      })
      describe('When the maximum number is above 0', () => {
        it('should return the correct result', () => {
          const minimum = -10
          const maximum = 10
          const actual = Fx.runSync(getRandomNumber(minimum, maximum))
          expect(actual).toBeGreaterThanOrEqual(minimum)
          expect(actual).toBeLessThanOrEqual(maximum)
        })
      })
    })
    describe('When the minimum number is above 0', () => {
      describe('When the maximum number is below 0', () => {
        it('should still work, regardless of which is higher', () => {
          const minimum = 1
          const maximum = -10
          const actual = Fx.runSync(getRandomNumber(minimum, maximum))
          expect(actual).toBeGreaterThanOrEqual(maximum)
          expect(actual).toBeLessThanOrEqual(minimum)
        })
      })
      describe('When the maximum number is above 0', () => {
        it('should return the correct result', () => {
          const minimum = 1
          const maximum = 10
          const actual = Fx.runSync(getRandomNumber(minimum, maximum))
          expect(actual).toBeGreaterThanOrEqual(minimum)
          expect(actual).toBeLessThanOrEqual(maximum)
        })
      })
    })
  })

  describe('decimalPlaces', () => {
    describe('When the number has no decimal places', () => {
      it('should return 0', () => {
        const actual = decimalPlaces(123)
        expect(actual).toEqual(0)
      })
    })
    describe('When the number has decimal places', () => {
      it('should return the amount of decimal places', () => {
        const actual = decimalPlaces(1.23)
        expect(actual).toEqual(2)
      })
    })
  })

  describe('scientificDecimalPlaces', () => {
    describe('When called with a scientific notation formatted number', () => {
      describe('When the number is negative', () => {
        it('should return the amount of decimal places', () => {
          const actual = scientificDecimalPlaces(-1e100)
          expect(actual).toEqual(100)
        })
      })
      describe('When the number is positive', () => {
        it('should return the amount of decimal places', () => {
          const actual = scientificDecimalPlaces(1e100)
          expect(actual).toEqual(100)
        })
      })
    })
  })

  describe('isScientificNotation', () => {
    describe('When called with a number', () => {
      it('should return false', () => {
        const actual = isScientificNotation(123)
        expect(actual).toEqual(false)
      })
    })
    describe('When called with a scientific notation formatter number', () => {
      it('should return true', () => {
        const actual = isScientificNotation(1e123)
        expect(actual).toEqual(true)
      })
    })
  })

  describe('isInteger', () => {
    describe('When called with a number that is not an integer', () => {
      it('should return false', () => {
        const actual = isInteger(1.23)
        expect(actual).toEqual(false)
      })
    })
    describe('When called with a number that is an integer', () => {
      it('should return true', () => {
        const actual = isInteger(123)
        expect(actual).toEqual(true)
      })
    })
  })

  describe('getPrecision', () => {
    describe('When called with a number that is an integer', () => {
      it('should return an Either.Left', () => {
        const actual = getPrecision(1)
        expect(actual).toEqual(Either.left('Not an integer'))
      })
    })
    describe('When called with a number that is not an integer', () => {
      describe('When the number is in decimal notation', () => {
        it('should return an Either.right with the correct value', () => {
          const actual = getPrecision(1.23)
          expect(actual).toEqual(Either.right(2))
        })
      })
      describe('When the number is in scientific notation', () => {
        describe('When the amount is above 6', () => {
          it('should return an Either.right with the correct value', () => {
            const actual = getPrecision(1e-7)
            expect(actual).toEqual(Either.right(7))
          })
        })
        describe('When the amount is below 7 (it is converted to decimal notation)', () => {
          it('should return an Either.right with the correct value', () => {
            const actual = getPrecision(1e-6)
            expect(actual).toEqual(Either.right(6))
          })
        })
      })
    })
  })
})
