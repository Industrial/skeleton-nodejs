import { describe, expect, it } from 'vitest'

import { asyncMap } from '.'

describe('asyncMap', () => {
  describe('When the mapping function returns a value', () => {
    it('should return the mapped values', async () => {
      const inputs = [1, 2, 3]
      const fn = async (input: number) => input * 2
      const result = await asyncMap(inputs, fn)
      expect(result).toEqual([2, 4, 6])
    })
  })

  describe('When the mapping function throws an error', () => {
    it('should reject the promise', async () => {
      const inputs = [1, 2, 3]
      const fn = async (input: number) => {
        if (input === 2) {
          throw new Error('Error')
        }
        return input * 2
      }
      await expect(asyncMap(inputs, fn)).rejects.toThrow('Error')
    })
  })

  describe('When the mapping function rejects the promise', () => {
    it('should reject the promise', async () => {
      const inputs = [1, 2, 3]
      const fn = async (input: number) => {
        if (input === 2) {
          return Promise.reject(new Error('Error'))
        }
        return input * 2
      }
      await expect(asyncMap(inputs, fn)).rejects.toThrow('Error')
    })
  })
})
