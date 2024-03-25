import { describe, expect, it } from 'vitest'

import { entries } from './record.ts'

describe('Record module', () => {
  describe('entries', () => {
    describe('When called with an empty object', () => {
      it('should return an empty array', () => {
        const actual = entries({})
        expect(actual).toStrictEqual([])
      })
    })

    describe('When called with an object with keys and values', () => {
      it('should return an array with entries of those keys and values', () => {
        const actual = entries({
          a: 'b',
          c: 'd',
        })

        expect(actual).toStrictEqual([
          ['a', 'b'],
          ['c', 'd'],
        ])
      })
    })
  })
})
