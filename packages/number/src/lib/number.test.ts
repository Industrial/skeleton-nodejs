import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import { describe, expect, it, test } from 'vitest'

import { factorial, factorialByIteration, getPrecision, getRandomNumber, safeDivide } from './number.ts'

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
      expect(O.isSome(result)).toBe(true)
      expect(O.getOrElse(() =>
        0)(result)).toBe(3)
    })

    test('should return none when the denominator is zero', () => {
      const result = safeDivide(6, 0)
      expect(O.isNone(result)).toBe(true)
    })

    test('should return none when both numbers are zero', () => {
      const result = safeDivide(0, 0)
      expect(O.isNone(result)).toBe(true)
    })

    test('should handle negative numbers properly', () => {
      const result = safeDivide(-6, 2)
      expect(O.isSome(result)).toBe(true)
      expect(O.getOrElse(() =>
        0)(result)).toBe(-3)
    })
  })

  describe('getNumberPrecision', () => {
    test('Integer', () => {
    // Arrange

      // Act
      const actual = getPrecision(1)

      // Assert
      expect(E.isLeft(actual)).toEqual(true)
    })

    test('Decimal Notation', () => {
    // Arrange
      const expected = 2

      // Act
      const actual = getPrecision(0.01)

      // Assert
      expect(actual).toEqual(E.right(expected))
    })

    describe('Scientific Notation', () => {
      test('Positive', () => {
      // Arrange

        // Act
        const actual = getPrecision(1e2)

        // Assert
        expect(E.isLeft(actual)).toEqual(true)
      })

      describe('When above 6', () => {
        test('Should return the exponent', () => {
        // Arrange
          const expected = 7

          // Act
          const actual = getPrecision(1e-7)

          // Assert
          expect(actual).toEqual(E.right(expected))
        })
      })

      describe('When above below 7 (it is converted to Decimal notation)', () => {
        test('Should return the exponent', () => {
        // Arrange
          const expected = 6

          // Act
          const actual = getPrecision(1e-6)

          // Assert
          expect(actual).toEqual(E.right(expected))
        })
      })
    })
  })

  describe('getRandomNumber function', () => {
    test('should return a number within the specified range', () => {
      const result = getRandomNumber(5, 10)
      expect(result).to.be.a('number')
      expect(result).to.be.at.least(5)
      expect(result).to.be.at.most(10)
    })

    test('should return the minimum value if minimum and maximum are the same', () => {
      const result = getRandomNumber(5, 5)
      expect(result).to.equal(5)
    })

    test('should handle negative range correctly', () => {
      const result = getRandomNumber(-10, -5)
      expect(result).to.be.a('number')
      expect(result).to.be.at.least(-10)
      expect(result).to.be.at.most(-5)
    })
  })
})
