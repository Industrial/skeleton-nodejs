import { describe, expect, it } from 'vitest'
import { series } from './series.ts'

describe('series', () => {
  describe('When the function succeeds', () => {
    it('should resolve with the results of the functions', async () => {
      const fn1 = async () => 'result1'
      const fn2 = async () => 'result2'
      const result = await series([fn1, fn2])
      expect(result).toEqual(['result1', 'result2'])
    })
  })

  describe('When any function fails', () => {
    it('should reject the promise', async () => {
      const fn1 = async () => 'result1'
      const fn2 = async () => {
        throw new Error('Error')
      }
      await expect(series([fn1, fn2])).rejects.toThrow('Error')
    })
  })
})
