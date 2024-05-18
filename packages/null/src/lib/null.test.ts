import { describe, expect, it } from 'bun:test'

import { isNull } from './null.ts'

describe('null', () => {
  describe('When called with a value that is not null', () => {
    it('should return false', () => {
      expect(isNull(123)).toEqual(false)
      expect(isNull('123')).toEqual(false)
      expect(isNull({})).toEqual(false)
      expect(isNull([])).toEqual(false)
      expect(isNull(undefined)).toEqual(false)
    })
  })
  describe('When called with a value that is null', () => {
    it('should return false', () => {
      expect(isNull(null)).toEqual(true)
    })
  })
})
