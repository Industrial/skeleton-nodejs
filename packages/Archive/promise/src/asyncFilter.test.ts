import { describe, expect, it } from 'vitest'

import { asyncFilter } from '.'

describe('asyncFilter', () => {
  describe('When all elements pass the predicate', () => {
    it('should return the filtered elements', async () => {
      const inputs = [1, 2, 3]
      const predicate = async (input: number) => input % 2 === 0
      const result = await asyncFilter(inputs, predicate)
      expect(result).toEqual([2])
    })
  })

  describe('When not all elements pass the predicate', () => {
    it('should return the filtered elements', async () => {
      const inputs = [1, 2, 3]
      const predicate = async (input: number) => input % 2 === 0
      const result = await asyncFilter(inputs, predicate)
      expect(result).toEqual([2])
    })
  })
})
