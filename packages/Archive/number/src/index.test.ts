import { describe, expect, it } from 'vitest'

import { clamp, factorial, isFloat, isInteger, roundToDecimalPlace } from '.'

describe('number', () => {
  describe('clamp', () => {
    describe('When the value is within the range', () => {
      it('should return the value', () => {
        const value = 5
        const min = 0
        const max = 10
        const result = clamp(value, min, max)
        expect(result).toBe(value)
      })
    })

    describe('When the value is less than the minimum', () => {
      it('should return the minimum', () => {
        const value = -5
        const min = 0
        const max = 10
        const result = clamp(value, min, max)
        expect(result).toBe(min)
      })
    })

    describe('When the value is greater than the maximum', () => {
      it('should return the maximum', () => {
        const value = 15
        const min = 0
        const max = 10
        const result = clamp(value, min, max)
        expect(result).toBe(max)
      })
    })
  })

  describe('factorial', () => {
    it('should return the factorial of the input', () => {
      const value = 5
      const result = factorial(value)
      expect(result).toBe(120)
    })
  })

  describe('isFloat', () => {
    describe('When the value is a float', () => {
      it('should return true', () => {
        const value = 5.5
        const result = isFloat(value)
        expect(result).toBe(true)
      })
    })

    describe('When the value is an integer', () => {
      it('should return false', () => {
        const value = 5
        const result = isFloat(value)
        expect(result).toBe(false)
      })
    })

    describe('When the value is NaN', () => {
      it('should return false', () => {
        const value = Number.NaN
        const result = isFloat(value)
        expect(result).toBe(false)
      })
    })
  })

  describe('isInteger', () => {
    describe('When the value is an integer', () => {
      it('should return true', () => {
        const value = 5
        const result = isInteger(value)
        expect(result).toBe(true)
      })
    })

    describe('When the value is a float', () => {
      it('should return false', () => {
        const value = 5.5
        const result = isInteger(value)
        expect(result).toBe(false)
      })
    })

    describe('When the value is NaN', () => {
      it('should return false', () => {
        const value = Number.NaN
        const result = isInteger(value)
        expect(result).toBe(false)
      })
    })
  })

  describe('roundToDecimalPlace', () => {
    describe('When the value is a float', () => {
      it('should round to the specified number of decimal places', () => {
        const value = 5.5555
        const decimals = 2
        const result = roundToDecimalPlace(value, decimals)
        expect(result).toBe(5.56)
      })
    })

    describe('When the value is an integer', () => {
      it('should round to the specified number of decimal places', () => {
        const value = 5
        const decimals = 2
        const result = roundToDecimalPlace(value, decimals)
        expect(result).toBe(5.0)
      })
    })

    describe('When the value is NaN', () => {
      it('should return NaN', () => {
        const value = Number.NaN
        const decimals = 2
        const result = roundToDecimalPlace(value, decimals)
        expect(result).toBe(Number.NaN)
      })
    })
  })
})
