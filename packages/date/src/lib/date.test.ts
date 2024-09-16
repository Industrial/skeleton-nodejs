import { describe, expect, it } from 'bun:test'
import { Option as O, pipe } from 'effect'

import { convertTime } from './date.ts'

export type TimeUnit = 'd' | 'h' | 'm' | 'ms' | 's'

describe('convertTime', () => {
  describe('When converting from ms to s', () => {
    it('should convert 1000 milliseconds to 1 second', () => {
      const actual = pipe(O.fromNullable(1000), O.flatMap(convertTime('ms', 's')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(1)
    })
  })

  describe('When converting from s to m', () => {
    it('should convert 60 seconds to 1 minute', () => {
      const actual = pipe(O.fromNullable(60), O.flatMap(convertTime('s', 'm')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(1)
    })
  })

  describe('When converting from m to h', () => {
    it('should convert 60 minutes to 1 hour', () => {
      const actual = pipe(O.fromNullable(60), O.flatMap(convertTime('m', 'h')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(1)
    })
  })

  describe('When converting from h to d', () => {
    it('should convert 24 hours to 1 day', () => {
      const actual = pipe(O.fromNullable(24), O.flatMap(convertTime('h', 'd')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(1)
    })
  })

  describe('When converting from d to h', () => {
    it('should convert 1 days to 24 hours', () => {
      const actual = pipe(O.fromNullable(1), O.flatMap(convertTime('d', 'h')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(24)
    })
  })

  describe('When converting from h to m', () => {
    it('should convert 1 hour to 60 minutes', () => {
      const actual = pipe(O.fromNullable(1), O.flatMap(convertTime('h', 'm')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(60)
    })
  })

  describe('When converting from m to s', () => {
    it('should convert 60 minutes to 3600 seconds', () => {
      const actual = pipe(O.fromNullable(60), O.flatMap(convertTime('m', 's')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(3600)
    })
  })

  describe('When converting from s to ms', () => {
    it('should convert 1 seconds to 1000 milliseconds', () => {
      const actual = pipe(O.fromNullable(1), O.flatMap(convertTime('s', 'ms')))
      if (O.isNone(actual)) {
        throw new Error('None')
      }
      expect(actual.value).toStrictEqual(1000)
    })
  })
})
