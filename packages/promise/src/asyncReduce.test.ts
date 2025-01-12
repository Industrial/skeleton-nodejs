import { describe, expect, it } from 'vitest'

import { asyncReduce } from './asyncReduce.ts'

describe('asyncReduce', () => {
  describe('When the reducer function returns a value', () => {
    it('should return the reduced value', async () => {
      const inputs = [1, 2, 3]
      const reducer = async (accumulator: number, input: number) =>
        accumulator + input
      const initialValue = 0
      const result = await asyncReduce(inputs, reducer, initialValue)
      expect(result).toBe(6)
    })
  })

  describe('When the reducer function throws an error', () => {
    it('should reject the promise', async () => {
      const inputs = [1, 2, 3]
      const reducer = async (accumulator: number, input: number) => {
        if (input === 2) {
          throw new Error('Error')
        }
        return accumulator + input
      }
      const initialValue = 0
      await expect(asyncReduce(inputs, reducer, initialValue)).rejects.toThrow(
        'Error',
      )
    })
  })

  describe('When the reducer function rejects the promise', () => {
    it('should reject the promise', async () => {
      const inputs = [1, 2, 3]
      const reducer = async (accumulator: number, input: number) => {
        if (input === 2) {
          return Promise.reject(new Error('Error'))
        }
        return accumulator + input
      }
      const initialValue = 0
      await expect(asyncReduce(inputs, reducer, initialValue)).rejects.toThrow(
        'Error',
      )
    })
  })
})
