import { describe, expect, it } from 'vitest'

import { asyncEvery } from './asyncEvery.ts'

describe('promise', () => {
  describe('asyncEvery', () => {
    describe('When all elements pass the predicate', () => {
      it('should return true', async () => {
        const inputs = [2, 4, 6]
        const predicate = async (input: number) => input % 2 === 0
        const result = await asyncEvery(inputs, predicate)
        expect(result).toBe(true)
      })
    })

    describe('When not all elements pass the predicate', () => {
      it('should return false', async () => {
        const inputs = [1, 2, 3]
        const predicate = async (input: number) => input % 2 === 0
        const result = await asyncEvery(inputs, predicate)
        expect(result).toBe(false)
      })
    })
  })
})
