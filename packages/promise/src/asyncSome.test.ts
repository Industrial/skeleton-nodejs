import { describe, expect, it } from 'vitest'

import { asyncSome } from './asyncSome.ts'

describe('asyncSome', () => {
  describe('When at least one element passes the predicate', () => {
    it('should return true', async () => {
      const inputs = [1, 2, 3]
      const predicate = async (input: number) => input % 2 === 0
      const result = await asyncSome(inputs, predicate)
      expect(result).toBe(true)
    })
  })

  describe('When no elements pass the predicate', () => {
    it('should return false', async () => {
      const inputs = [1, 3, 5]
      const predicate = async (input: number) => input % 2 === 0
      const result = await asyncSome(inputs, predicate)
      expect(result).toBe(false)
    })
  })
})
