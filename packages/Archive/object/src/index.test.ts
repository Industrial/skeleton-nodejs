import { describe, expect, it } from 'vitest'
import { isObject, mapKeys, valuesToLowerCase } from './index'

describe('isObject', () => {
  describe('When the value is an object', () => {
    it('should identify the value as an object', () => {
      const actual = isObject({ foo: 'bar' })
      const expected = true
      expect(actual).toBe(expected)
    })
  })

  describe('When the value is null', () => {
    it('should not identify the value as an object', () => {
      const actual = isObject(null)
      const expected = false
      expect(actual).toBe(expected)
    })
  })

  describe('When the value is an array', () => {
    it('should not identify the value as an object', () => {
      const actual = isObject([])
      const expected = false
      expect(actual).toBe(expected)
    })
  })

  describe('When the value is a primitive', () => {
    it('should not identify the value as an object', () => {
      const actual = isObject(1)
      const expected = false
      expect(actual).toBe(expected)
    })
  })
})

describe('mapKeys', () => {
  describe('When the input is an object', () => {
    describe('When the transform function is defined', () => {
      it('should apply the transform function to all keys', () => {
        const actual = mapKeys({ foo: 'bar' }, (key) => key.toUpperCase())
        const expected = { FOO: 'bar' }
        expect(actual).toEqual(expected)
      })
    })

    describe('When the transform function is an identity function', () => {
      it('should keep all keys unchanged', () => {
        const actual = mapKeys({ foo: 'bar' }, (key) => key)
        const expected = { foo: 'bar' }
        expect(actual).toEqual(expected)
      })
    })
  })
})

describe('valuesToLowerCase', () => {
  describe('When the input is an object', () => {
    describe('When all values are strings', () => {
      it('should convert all string values to lowercase', () => {
        const actual = valuesToLowerCase({ foo: 'SpOnGeBoB' })
        const expected = { foo: 'spongebob' }
        expect(actual).toEqual(expected)
      })
    })

    describe('When some values are not strings', () => {
      it('should convert only string values to lowercase', () => {
        const actual = valuesToLowerCase({ foo: 'SpOnGeBoB', bar: 123 })
        const expected = { foo: 'spongebob', bar: 123 }
        expect(actual).toEqual(expected)
      })
    })

    describe('When object contains nested objects', () => {
      it('should convert string values to lowercase within nested objects', () => {
        const actual = valuesToLowerCase({ foo: { bar: 'SpOnGeBoB' } })
        const expected = { foo: { bar: 'spongebob' } }
        expect(actual).toEqual(expected)
      })
    })

    describe('When object contains arrays', () => {
      it('should convert string values to lowercase within arrays', () => {
        const actual = valuesToLowerCase({ foo: ['SpOnGeBoB'] })
        const expected = { foo: ['spongebob'] }
        expect(actual).toEqual(expected)
      })
    })
  })
})
