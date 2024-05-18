import { describe, expect, it } from 'bun:test'

import { isUndefined } from './undefined.ts'

describe('undefined', () => {
  describe('When called with a value that is not null', () => {
    it('should return false', () => {
      expect(isUndefined(123)).toEqual(false)
      expect(isUndefined('123')).toEqual(false)
      expect(isUndefined({})).toEqual(false)
      expect(isUndefined([])).toEqual(false)
      expect(isUndefined(null)).toEqual(false)
    })
  })
  describe('When called with a value that is null', () => {
    it('should return false', () => {
      expect(isUndefined(undefined)).toEqual(true)
    })
  })
})
